package scala.tools.eclipse
package refactoring

import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange
import org.eclipse.ltk.core.refactoring.RefactoringStatus
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.tools.eclipse.refactoring.ui._
import scala.tools.nsc.util.SourceFile
import scala.tools.refactoring.analysis.{GlobalIndexes, Indexes, NameValidation}
import scala.tools.refactoring.common.{ConsoleTracing, InteractiveScalaCompiler, Selections}
import scala.tools.refactoring.implementations.Rename
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.implementations.MoveClass
import org.eclipse.ltk.ui.refactoring.resource.MoveResourcesWizard
import org.eclipse.ltk.ui.refactoring.RefactoringWizardPage
import org.eclipse.ltk.internal.core.refactoring.resource.MoveResourcesProcessor
import org.eclipse.core.resources.IContainer
import org.eclipse.jdt.internal.ui.refactoring.reorg.ReorgUserInputPage
import org.eclipse.jdt.core.IPackageFragment
import org.eclipse.ltk.core.refactoring.CompositeChange
import org.eclipse.ltk.core.refactoring.resource.MoveResourceChange
import org.eclipse.core.resources.IFolder
import org.eclipse.jdt.internal.corext.refactoring.nls.changes.CreateFileChange
import org.eclipse.core.runtime.Path
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.analysis.Indexes

/**
 * 
 */
trait FullProjectIndex {
  
  val refactoring: MultiStageRefactoring with InteractiveScalaCompiler with GlobalIndexes    
    
  val file: ScalaSourceFile
  
  /**
   * A cleanup handler, will later be set by the refactoring
   * to remove all loaded compilation units from the compiler.
   */
  type CleanupHandler = () => Unit
  
  def buildFullProjectIndex(pm: IProgressMonitor): (refactoring.IndexLookup, CleanupHandler) = {

    val allProjectSourceFiles = file.project.allSourceFiles.toList
    
    def collectAllScalaSources(files: List[IFile]) = {
      val allScalaSourceFiles = files flatMap { f =>
        ScalaSourceFile.createFromPath(f.getFullPath.toString)
      }
      
      allScalaSourceFiles map { ssf => 
        ssf.withSourceFile { (sourceFile, _) => sourceFile
        }()
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

      pm.subTask("Loading source files.")

      val r = new refactoring.global.Response[Unit]
      refactoring.global.askReload(files, r)
      r.get
      
      files flatMap { f =>
        if(pm.isCanceled) {
          None
        } else {
          val r = new refactoring.global.Response[refactoring.global.Tree]
          refactoring.global.askType(f, forceReload = false, r)
          Some(r)
        }
      }        
    }
    
    /**
     * Waits until all the typechecking has finished. Every 200 ms, it is checked
     * whether the user has canceled the process.
     */
    def typeCheckAll(responses: List[refactoring.global.Response[refactoring.global.Tree]], pm: IProgressMonitor) = {
      
      import refactoring.global._
      
      def waitForResultOrCancel(r: Response[Tree]) = {

        var result = None: Option[Tree]
        
        do {
          if (pm.isCanceled) r.cancel()
          else r.get(200) match {
            case Some(Left(data)) if r.isComplete /*no provisional results*/ => 
              result = Some(data)
            case _ => // continue waiting
          }
        } while (!r.isComplete && !r.isCancelled)
          
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
            
    // we need to store the already loaded files so that don't
    // remove them from the presentation compiler later.
    val previouslyLoadedFiles = refactoring.global.unitOfFile.values map (_.source) toList
    
    val files = collectAllScalaSources(allProjectSourceFiles)
    
    val responses = mapAllFilesToResponses(files, pm)
    
    pm.subTask("typechecking source files")
    
    val trees = typeCheckAll(responses, pm)
    
    // will be called after the refactoring has finished
    val cleanup = { () => 
      (files filterNot previouslyLoadedFiles.contains) foreach {
        refactoring.global.removeUnitOf
      }
    }
    
    if(!pm.isCanceled) {
      
      pm.subTask("creating index")
      
      val cus = trees map refactoring.CompilationUnitIndex.apply
      
      (refactoring.GlobalIndex(cus), cleanup)
    } else {
      refactoring.GlobalIndex(Nil) -> cleanup
    }

/*
    val status = super.checkInitialConditions(pm)
    
    if(pm.isCanceled) {
      status.addWarning("Indexing was cancelled, not all references might get adapted.")
    }

    status*/
  }
}
