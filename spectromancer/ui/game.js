// Select and remove 'n' elements from 'xs', randomly
function choose(n,xs) {
  var len = xs.length
  var chosen = []
  while (n > 0 && len > 0) {
    var ix = Math.trunc(Math.random() * len)
    chosen.push(xs.splice(ix,1)[0])
    --n
    --len
  }
  return chosen
}

// Create two new players of the given classes.
function newPlayers(t1,t2) {
  var newdeck = jQuery.extend(true, {}, cards)
  var fire  = newdeck['FIRE CARDS']
  var water = newdeck['WATER CARDS']
  var earth = newdeck['EARTH CARDS']
  var air   = newdeck['AIR CARDS']

  function newPlayer(p) {
    return { health: 50
           , fire: 5
           , water: 5
           , earth: 5
           , air: 5
           , fireCards: choose(4,fire)
           , waterCards: choose(4,water)
           , airCards: choose(4,air)
           , earthCards: choose(4,earth)
           , specialCards: choose(4,newdeck[p])
           }
  }

  return [ newPlayer(t1), newPlayer(t2) ]
}


