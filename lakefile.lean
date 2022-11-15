import Lake
open Lake DSL System

package widgetKit {
  -- add package configuration options here
}

lean_lib WidgetKit {
  -- add library configuration options here
}

@[default_target]
lean_exe widgetKit {
  root := `Main
}

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
  let jsFile := widgetDir / "dist" / s!"{tsxName}.js"
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
  let dependencies : Array FilePath := #[] -- ← add extra deps here
  let dependencies ← dependencies.mapM fun f => inputFile (widgetDir / "src" / f)
  let items := #["staticHtml"]
  let xs ← items.mapM (fun item => widgetTsxTarget pkg item dependencies)
  BuildJob.collectArray xs

