import Lean

open Lean Elab Command System  

unsafe def main (args : List String) : IO UInt32 := do
  let some hash := args[0]? | throw <| .userError "Usage: lake exe grade {hash}"
  let some hash := hash.toNat? | throw <| .userError s!"Failed to parse hash {hash}"
  let hash : UInt64 := hash.toUInt64
  let solutionFile : FilePath := "Solution.lean"
  let solutionContents ← IO.FS.readFile solutionFile
  let inputCtx := Parser.mkInputContext solutionContents "<input>"
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  enableInitializersExecution
  initSearchPath (← findSysroot)
  let (env, messages) ← processHeader header {} messages inputCtx
  let cmdState : Command.State := Command.mkState env messages {}
  let frontEndState ← IO.processCommands inputCtx parserState cmdState
  let env := frontEndState.commandState.env
  let axioms : Name → (Array Name) := fun nm => (CollectAxioms.collect nm |>.run env |>.run {}).snd.axioms
  let some solution := env.find? `solution | 
    IO.println "Solution not found in environment."
    return 1
  if solution.type.hash != hash then
    IO.println "Solution has wrong type."
    return 1
  if (axioms solution.name).size != 0 then
    IO.println "Solution depends on axioms."
    return 1
  IO.println "Solution is valid."
  return 0

elab "#type_hash" id:ident : command => do
  let id ← resolveGlobalConstNoOverload id
  let env ← getEnv
  let some cinfo := env.find? id | throwError s!"{id} not found in environment."
  IO.println cinfo.type.hash