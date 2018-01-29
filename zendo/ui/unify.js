var nextVarName = 0

function newVar() {
  return { uvar: nextVarName++ }
}

function con() {
  var xs = []
  var n = arguments.length;
  if (n < 1) return null
  for (var i = 1; i < n; ++i) xs[i - 1] = arguments[i]
  return { conName: arguments[0] , children: xs }
}


// Apply the substitution, returns `null` if no changes.
function apSubstMaybe(su,t) {
  var x = t.uvar
  if (x !== undefined) {
    var t1 = su[x]
    return t1? t1 : null
  }

  var changes = false
  var xs = t.children
  var n = xs.length
  if (n === 0) return null

  var ys = []
  for (var i = 0; i < n; ++i) {
    var t = apSubstMaybe(su,xs[i])
    if (t) {
      changes = true
      ys[i] = t
    } else {
      ys[i] = xs[i]
    }
  }

  if (!changes) return null
  return { conName: t.conName, children: ys }
}

function apSubst(su,t) {
  var x = apSubstMaybe(su,t)
  return x? x : t
}

// Modifies the substitution.
// Returns `true` if successful.
// If it fails, the substitution
// might still be partially modified.
function unify(su,t1,t2) {
  var t1 = apSubst(su,t1)
  var t2 = apSubst(su,t2)
  var x  = t1.uvar
  if (x !== undefined) return bindVar(su,x,t2)
  x = t2.uvar
  if (x !== undefined) return bindVar(su,x,t1)

  var c1 = t1.conName
  var c2 = t2.conName
  if (c1 !== c2) return false

  var xs = t1.children
  var ys = t2.children
  var n = xs.length
  if (n !== ys.length) return false

  for (var i = 0; i < n; ++i) {
    var ok = unify(su,xs[i],ys[i])
    if (!ok) return false
  }
  return true
}

function bindVar(su,x,t1) {

  if (t1.uvar === x) return true

  function occurs(t) {
    if (t.uvar === x) return true
    var xs = t.children
    var n = xs.length
    for (var i = 0; i < n; ++i) {
      if ( occurs(xs[i]) ) return true
    }
    return false
  }
  if (occurs(t1)) return false
  su[x] = t1
  return true
}


function showType(t) {
  var x = t.uvar
  if (x !== undefined) {
    var r = Math.floor(x / 26)
    var m = String.fromCharCode(97 + (x % 26))
    return '?' + m + (r > 0? r : '')
  }

  var s = t.conName
  var xs = t.children
  var n = xs.length
  if (n === 0) return s

  function isAtom(ty) {
    return ty.uvar !== undefined || ty.children.length === 0 }

  for (var i = 0; i < n; ++i) {
    s += ' '
    var t = xs[i]
    var x = showType(t)
    if (isAtom(t)) s += x
    else s += '(' + x + ')'
  }
  return s
}
