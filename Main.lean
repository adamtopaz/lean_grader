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

def Lean.Environment.axioms (env : Environment) (nm : Name) : 
    Array Name :=  
  (CollectAxioms.collect nm |>.run env |>.run {}).snd.axioms

unsafe def runSaveCmd (p : Parsed) : IO UInt32 := do
  let path : String := p.positionalArg! "input" |>.as! String
  IO.println path
  let path : FilePath := path
  let name : String := p.positionalArg! "name" |>.as! String
  IO.println name
  let name : Name := .mkSimple name
  let env ← getSolutionEnv
  let some sol := env.find? `solution | 
    throw <| .userError "Solution with name `solution` not found in environment."
  let tp := sol.type
  if p.hasFlag "check" && (env.axioms `solution).size != 0 then
    throw <| .userError "Solution depends on axioms."
  pickle path tp name
  IO.println s!"Saved the following expression to file:
{tp}"
  return 0

def Lean.Environment.compareExpr (env : Environment) (e1 e2 : Expr) : IO Bool := do
  let e := Meta.isDefEq e1 e2
  ContextInfo.runMetaM
    { env := env, fileMap := default, ngen := {} } {} e

unsafe def runCompareCmd (p : Parsed) : IO UInt32 := do
  let paths : Array String := p.variableArgsAs! String
  let paths : Array FilePath := paths.map .mk
  let exprs ← paths.mapM fun f => unpickle Expr f
  let exprs := exprs.toList
  let env ← getSolutionEnv
  let test : Bool ← show IO Bool from do 
    match exprs with
      | [] => return true
      | (expr, _) :: exprs => 
        let exprs : List Bool ← exprs.mapM fun (e, _) => 
          env.compareExpr expr e
        return exprs.foldl (· && ·) true
  return if test then 0 else 1

unsafe def save := `[Cli| 
  save VIA runSaveCmd;
  "Save the solution to a file."
  FLAGS:
    c, check; "Checcks whether solution depends on axioms."
  ARGS:
    path : String; "Filepath to use to save solution Expr."
    name : String; "A name needed for pickle procedure."
]

unsafe def compare := `[Cli| 
  compare VIA runCompareCmd;
  "Compare two solutions from file."
  ARGS:
    ...inputs : String; "Inputs to compare."]

unsafe def mainCommand := `[Cli| 
  grade NOOP; ["0.0.1"]
  "The Lean Grader CLI tool."
  SUBCOMMANDS:
    save;
    compare
]

unsafe def main (args : List String) : IO UInt32 :=
  mainCommand.validate args