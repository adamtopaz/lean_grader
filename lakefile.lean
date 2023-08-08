import Lake
open Lake DSL

package «lean_grader» {
  -- add package configuration options here
}

lean_lib «LeanGrader» {
  -- add library configuration options here
}

@[default_target]
lean_exe «grade» {
  root := `Main
}
