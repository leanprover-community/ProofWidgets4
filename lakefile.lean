import Lake
open Lake DSL System

package widgetKit {
  preferReleaseBuild := true
}

lean_lib WidgetKit {}

/-! Widget build -/

def npmCmd : String :=
  if Platform.isWindows then "npm.cmd" else "npm"

def widgetDir := __dir__ / "widget"

target widgetPackageLock : FilePath := do
  let packageFile ← inputFile <| widgetDir / "package.json"
  let packageLockFile := widgetDir / "package-lock.json"
  buildFileAfterDep packageLockFile packageFile fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["install"]
      cwd := some widgetDir
    }

def widgetTsxTarget (pkg : Package) (tsxName : String) (deps : Array (BuildJob FilePath))
    [Fact (pkg.name = _package.name)] : IndexBuildM (BuildJob FilePath) := do
  let jsFile := pkg.buildDir / "js" / s!"{tsxName}.js"
  let deps := deps ++ #[
    ← inputFile <| widgetDir / "src" / s!"{tsxName}.tsx",
    ← inputFile <| widgetDir / "rollup.config.js",
    ← inputFile <| widgetDir / "tsconfig.json",
    ← fetch (pkg.target ``widgetPackageLock)
  ]
  buildFileAfterDepArray jsFile deps fun _srcFile => do
    proc {
      cmd := npmCmd
      args := #["run", "build", "--", "--tsxName", tsxName]
      cwd := some widgetDir
    }

target widgets (pkg : Package) : Array FilePath := do
  let fs ← (widgetDir / "src").readDir
  let tsxs : Array FilePath := fs.filterMap fun f =>
    let p := f.path; if let some "tsx" := p.extension then some p else none
  -- Conservatively, every .js build depends on all the .tsx source files.
  let deps ← liftM <| tsxs.mapM inputFile
  let jobs ← tsxs.mapM fun tsx => widgetTsxTarget pkg tsx.fileStem.get! deps
  BuildJob.collectArray jobs

@[default_target]
target all (pkg : Package) : Unit := do
  let some lib := pkg.findLeanLib? ``WidgetKit |
    error "cannot find lean_lib target"
  let job₁ ← fetch (pkg.target ``widgets)
  let _ ← job₁.await
  let job₂ ← lib.recBuildLean
  let _ ← job₂.await
  return .nil
