package scala.tools.eclipse.ui

import org.eclipse.debug.core.ILaunch

class ScalaTestRunSession(fLaunch: ILaunch, fRunName: String) {
  //private var fLaunch: ILaunch
  //private var 
  
  var startedCount = 0
  var succeedCount = 0
  var failureCount = 0
  var ignoredCount = 0
  var pendingCount = 0
  var canceledCount = 0
  var totalCount = 0
  var suiteCount = 0
  var suiteAbortedCount = 0
  
  var rootNode: RunModel = null
  
  private var running = false
  
  def run() {
    running = true
  }
  
  def stop() {
    running = false
  }
  
  def isStopped = false  // should change when user stop it
  
  def isRunning = running
}