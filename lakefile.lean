import Lake
open Lake DSL

package «lean_grader» {
  -- add package configuration options here
}

lean_lib «LeanGrader» {
  -- add library configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[default_target]
lean_exe «grade» {
  supportInterpreter := true
  root := `Main
}
