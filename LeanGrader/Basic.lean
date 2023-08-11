import Lean

open Lean

elab "#type_hash" id:ident : command => do
  let id ← resolveGlobalConstNoOverload id
  let env ← getEnv
  let some cinfo := env.find? id | throwError s!"{id} not found in environment."
  IO.println cinfo.type.hash