import Cli
import Lean
import Std.Util.Pickle

open Cli Lean System Elab Command

unsafe def getSolutionEnv : IO Environment := do
  let solutionFile : FilePath := "Solution.lean"
  let solutionContents ← IO.FS.readFile solutionFile
  let inputCtx := Parser.mkInputContext solutionContents "<input>"
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  enableInitializersExecution
  initSearchPath (← findSysroot)
  let (env, messages) ← processHeader header {} messages inputCtx
  let cmdState : Command.State := Command.mkState env messages {}
  let frontEndState ← IO.processCommands inputCtx parserState cmdState
  return frontEndState.commandState.env

unsafe def runSaveEnvCmd (p : Parsed) : IO UInt32 := do
  let path : String := p.positionalArg! "path" |>.as! String
  let path : FilePath := path
  let name : String := p.positionalArg! "name" |>.as! String
  let name : Name := .mkSimple name
  let env ← getSolutionEnv 
  pickle path env name
  return 0

def Lean.Environment.axioms (env : Environment) (nm : Name) : 
    Array Name :=  
  (CollectAxioms.collect nm |>.run env |>.run {}).snd.axioms

unsafe def runSaveTypeCmd (p : Parsed) : IO UInt32 := do
  let path : String := p.positionalArg! "path" |>.as! String
  let path : FilePath := path
  let name : String := p.positionalArg! "name" |>.as! String
  let name : Name := .mkSimple name
  let env ← getSolutionEnv
  let some sol := env.find? `solution | 
    throw <| .userError "Solution with name `solution` not found in environment."
  let tp := sol.type
  pickle path tp name
  IO.println s!"Saved the following expression to file:
{tp}"
  return 0

unsafe def runSaveValCmd (p : Parsed) : IO UInt32 := do
  let path : String := p.positionalArg! "path" |>.as! String
  let path : FilePath := path
  let name : String := p.positionalArg! "name" |>.as! String
  let name : Name := .mkSimple name
  let env ← getSolutionEnv
  if (env.axioms `solution).size != 0 then
    throw <| .userError "Solution depends on axioms."
  let some sol := env.find? `solution | 
    throw <| .userError "Solution with name `solution` not found in environment."
  let some val := sol.value? | 
    throw <| .userError "Solution with name `solution` has no value."
  pickle path val name
  IO.println s!"Saved the following expression to file:
{val}"
  return 0

def Lean.Environment.compareExpr (env : Environment) (e1 e2 : Expr) : IO Bool := do
  let e := Meta.isDefEq e1 e2
  ContextInfo.runMetaM
    { env := env, fileMap := default, ngen := {} } {} e

unsafe def runCheckCmd (p : Parsed) : IO UInt32 := do
  let env : String := p.positionalArg! "env" |>.as! String
  let type : String := p.positionalArg! "type" |>.as! String
  let val : String := p.positionalArg! "value" |>.as! String
  let (env, _) ← unpickle Environment env 
  let (type, _) ← unpickle Expr type
  let (val, _) ← unpickle Expr val
  let actualType ← ContextInfo.runMetaM { env := env, fileMap := default, ngen := {} } {} 
    (Meta.inferType val)
  if !(← env.compareExpr type actualType) then
    throw <| .userError "Failed!"
  IO.println "Success!"
  return 0

unsafe def saveEnv := `[Cli|
  save_env VIA runSaveEnvCmd;
  "Save the current solution environment to a file."
  ARGS:
    path : String; "Filepath to use to save solution Expr."
    name : String; "A name needed for pickle procedure."
]

unsafe def saveType := `[Cli| 
  save_type VIA runSaveTypeCmd;
  "Save the solution type to a file."
  ARGS:
    path : String; "Filepath to use to save solution Expr."
    name : String; "A name needed for pickle procedure."
]

unsafe def saveVal := `[Cli| 
  save_val VIA runSaveValCmd;
  "Save the solution value to a file."
  ARGS:
    path : String; "Filepath to use to save solution Expr."
    name : String; "A name needed for pickle procedure."
]

unsafe def check := `[Cli| 
  check VIA runCheckCmd;
  "Check that a value has the correct type."
  ARGS:
    env : String; "File containing the environment."
    value : String; "File containing the value expr."
    type : String; "File containing the type expr."
]

unsafe def mainCommand := `[Cli| 
  grade NOOP; ["0.0.1"]
  "The Lean Grader CLI tool."
  SUBCOMMANDS:
    saveType;
    saveVal;
    saveEnv;
    check
]

unsafe def main (args : List String) : IO UInt32 :=
  mainCommand.validate args