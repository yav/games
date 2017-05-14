

function drawYield(y) {
  switch(y.tag) {
    case 'immediate': return drawImmediateYield(y.outputs)
    case 'long_term': return drawLongTermYield(y.outputs)

      Immediate as -> JS.object [ jsTag "immediate", "outputs" .= as ]
      LongTerm a   -> JS.object [ jsTag "long_term", "outputs" .= a ]
  }
}
