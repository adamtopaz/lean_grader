import Lean

open Lean Elab Command

namespace LeanGrader

structure Problem where
  name : Name
  hash : UInt64
deriving ToJson, FromJson

elab "#hash" ids:ident,* : command => do
  let ids := ids.getElems
  let ids ← ids.mapM resolveGlobalConstNoOverload
  let env ← getEnv
  let some cs := ids.mapM env.find? | throwError s!"{ids} not all found in environment."
  let cs := cs.map fun c => toJson (Problem.mk c.name c.type.hash) |>.compress
  for c in cs do
    IO.println c

elab "#save_problems" fname:str "[" ids:ident,* "]" : command => do
  let fname := fname.getString
  let ids := ids.getElems
  let ids ← ids.mapM resolveGlobalConstNoOverload
  let env ← getEnv
  let some cs := ids.mapM env.find? | throwError s!"{ids} not all found in env."
  let cs : Array Problem := cs.map fun c => ⟨c.name, c.type.hash⟩
  IO.FS.withFile fname .write fun handle => do
    for c in cs do
      handle.putStrLn (toJson c).compress

end LeanGrader