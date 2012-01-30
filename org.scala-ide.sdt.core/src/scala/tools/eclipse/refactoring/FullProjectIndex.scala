package scala.tools.eclipse
package refactoring

import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor

import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.util.HasLogger
import scala.tools.nsc.util.SourceFile
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.MultiStageRefactoring

/**
 * A trait that can be mixed into refactorings that need an index of the whole 
 * project (e.g. Global Rename, Move Class).
 * 
 * This loads all the files in the project into the presentation compiler, which
 * takes significant time. Once the Scala IDE has its own index, we should be able
 * to make this much more efficient than it currently is.
 */
trait FullProjectIndex extends HasLogger {
  
  val refactoring: MultiStageRefactoring with InteractiveScalaCompiler with GlobalIndexes    
    
  val project: ScalaProject
  
  lazy val allProjectSourceFiles = project.allSourceFiles.toList

  /**
   * A cleanup handler, will later be set by the refactoring
   * to remove all loaded compilation units from the compiler.
   */
  type CleanupHandler = () => Unit
  
  /**
   * Builds an index from all the source files in the current project. The returned 
   * CleanupHandler needs to be called when the index isn't used anymore, this will
   * then unload all the originally unloaded files from the presentation compiler.
   */
  def buildFullProjectIndex(pm: IProgressMonitor): (refactoring.IndexLookup, CleanupHandler) = {
    
    import refactoring.global
    
    def collectAllScalaSources(files: List[IFile]): List[SourceFile] = {
      val allScalaSourceFiles = files flatMap { f =>
        if(pm.isCanceled)
          return Nil
        else 
          ScalaSourceFile.createFromPath(f.getFullPath.toString)
      }
      
      allScalaSourceFiles map { ssf =>
        if(pm.isCanceled)
          return Nil
        else
          ssf.withSourceFile { (sourceFile, _) => sourceFile}()
      }
    }
    
    /**
     * First loads all the source files into the compiler and then starts
     * typeckecking them. The method won't block until typechecking is done
     * but return all the Response objects instead.
     * 
     * If the process gets canceled, no more new typechecks will be started.
     */
    def mapAllFilesToResponses(files: List[SourceFile], pm: IProgressMonitor) = {

      pm.subTask("loading dependent source files")

      val r = new global.Response[Unit]
      global.askReload(files, r)
      r.get
      
      files flatMap { f =>
        if(pm.isCanceled) {
          None
        } else {
          val r = new global.Response[global.Tree]
          global.askType(f, forceReload = false /*we just loaded the files*/, r)
          Some(r)
        }
      }        
    }
    
    /**
     * Waits until all the typechecking has finished. Every 200 ms, it is checked
     * whether the user has canceled the process.
     */
    def typeCheckAll(responses: List[global.Response[global.Tree]], pm: IProgressMonitor) = {
      
      
      def waitForResultOrCancel(r: global.Response[global.Tree]) = {

        var result = None: Option[global.Tree]
        
        do {
          if (pm.isCanceled) r.cancel()
          else r.get(200) match {
            case Some(Left(data)) if r.isComplete /*no provisional results*/ => 
              result = Some(data)
            case _ => // continue waiting
          }
        } while (!r.isComplete && !r.isCancelled && !pm.isCanceled)
          
        result
      }
      
      responses flatMap { 
        case r if !pm.isCanceled => 
          waitForResultOrCancel(r)
        case r =>
          None
      }
    }
          
    pm.beginTask("loading files: ", 3)
            
    // we need to store the already loaded files so that we don't
    // remove them from the presentation compiler later.
    val previouslyLoadedFiles = global.unitOfFile.values map (_.source) toList
    
    val files = collectAllScalaSources(allProjectSourceFiles)
    
    val responses = mapAllFilesToResponses(files, pm)
    
    pm.subTask("typechecking source files")
    
    val trees = typeCheckAll(responses, pm)
    
    // will be called after the refactoring has finished
    val cleanup = { () => 
      (files filterNot previouslyLoadedFiles.contains) foreach {
        global.removeUnitOf
      }
    }
    
    val cus = if(!pm.isCanceled) {
      
      pm.subTask("creating index")
      
      trees flatMap { tree =>
        try {
          global.ask { () =>
            Some(refactoring.CompilationUnitIndex(tree))
          }
        } catch {
          case t => 
            logger.error(t)
            None
        }
      }
    } else Nil
    
    (refactoring.GlobalIndex(cus), cleanup)
  }
}
