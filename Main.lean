import «LeanGrader»

open Lean Elab Command System LeanGrader 

def main (args : List String) : IO UInt32 := do
  let some problemFile := args[0]? | throw <| .userError "Usage: grade {problemFile}.jsonl {solutionFile}.lean"
  let some solutionFile := args[1]? | throw <| .userError "Usage: grade {problemFile}.jsonl {solutionFile}.lean"
  let lines ← IO.FS.lines ⟨problemFile⟩
  let .ok probs := lines.mapM Json.parse | throw <| .userError "Failed to parse lines as json."
  let .ok (probs : Array Problem) := probs.mapM fromJson? | throw <| .userError "Failed to parse lines as problems."
  let solutionContents ← IO.FS.readFile ⟨solutionFile⟩
  let inputCtx := Parser.mkInputContext solutionContents "<input>"
  let (header, _, messages) ← Parser.parseHeader inputCtx
  let (env, _) ← processHeader header {} messages inputCtx
  let axioms : Name → (Array Name) := fun nm => (CollectAxioms.collect nm |>.run env |>.run {}).snd.axioms
  for p in probs do
    if (axioms p.name).size != 0 then return 1
  return 0