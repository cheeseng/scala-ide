package scala.tools.eclipse.launching

import org.eclipse.core.expressions.PropertyTester
import scala.tools.eclipse.javaelements.ScalaSourceFile
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.ITypeHierarchy
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.jdt.internal.core.PackageFragment
import scala.tools.eclipse.ScalaSourceFileEditor

class ScalaTestLaunchableTester extends PropertyTester {
  
  def test(receiver: Object, property: String, args: Array[Object], expectedValue: Object): Boolean = {
    println("#####receiver: " + receiver.getClass.getName)
    if(receiver.isInstanceOf[IAdaptable]) {
      val je = receiver.asInstanceOf[IAdaptable].getAdapter(classOf[IJavaElement]).asInstanceOf[IJavaElement]
      canLaunchAsScalaTest(je)
    }
    else
      false
  }
  
  private def canLaunchAsScalaTest(element: IJavaElement): Boolean = {
    try {
      ScalaTestLaunchShortcut.containsScalaTestSuite(element)
    } catch {
      case e:Exception => false
    }
  }
}

class ScalaTestPackageTester extends PropertyTester {
  
  def test(receiver: Object, property: String, args: Array[Object], expectedValue: Object): Boolean = {
    receiver match {
      case packageFragment: PackageFragment => 
        true
      case _ => 
        false
    }
  }
}

class ScalaTestTestTester extends PropertyTester {
  
  def test(receiver: Object, property: String, args: Array[Object], expectedValue: Object): Boolean = {
    receiver match {
      case scEditor: ScalaSourceFileEditor => 
        try {
          val selectionOpt = ScalaTestLaunchShortcut.resolveSelectedAst(scEditor.getEditorInput, scEditor.getEditorSite.getSelectionProvider)
          selectionOpt match {
            case Some(selection) => 
              true
            case None => 
              false
          }
        }
        catch {
          case _ => false
        }
      case _ =>
        false
    }
  }
}