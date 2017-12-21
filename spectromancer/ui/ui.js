var selected = null
var gameID = null
var helpMode = false
var oldGame = null
var errMsg = null

var stats = { caster: {}, opponent: {} }

function makeTurn(url,info,tgt) {
  var newG = null
  var animFinished = false

  function draw() {

    drawEvents(newG.log, function() {
      $('body').empty().append( drawNewGame(newG.winner)
                              , drawGame(newG.game,newG.history,newG.winner))
      scrollHist()
    })
  }

  if (tgt !== undefined) {
    finished = false
    var loc = $(tgt).offset()
    selected.dom
      .css('position','absolute')
      .css('z-index','100')
      .animate({ left: loc.left, top: loc.top }, 'slow', 'swing', function() {
         selected.dom.hide()
         if (newG !== null) draw()
         else animFinished = true
      })
  } else animFinished = true

  jQuery.extend(info, { gid: gameID })
  jQuery.post(url, info, function(res) {
    errMsg = null
    oldGame = res
    newG = res
    if (animFinished) draw()
  }).fail(function(err) {
    console.log(err.statusText)
    errMsg = err.statusText
    newG = oldGame
    newG.log = []
    draw()
  });


}

function scrollHist() {
  var x = $('#history')
  x.scrollTop(x[0].scrollHeight)
}

function drawHistory(h) {
  var dom = $('<div/>')
            .attr('id','history')
            .css('text-align','left')
            .css('max-width','30em')
            .css('height','30em')
            .css('background-color','rgba(0,0,0,0.75)')
            .css('display','block')
            .css('overflow','auto')
            .css('float','right')
            .css('padding','1em')
            .css('border-radius','10px')

  function locToText(l) {
    switch (l.who) {
      case 'caster':   return 'owner\'s ' + l.slot
      case 'opponent': return 'opponent\'s ' + l.slot
      default: return '(unknown location)'
    }
  }

  function drawMove(m) {
    var msg
    switch (m.tag) {
      case 'skipTurn': msg = 'Skip turn'; break
      case 'playCard':
        msg = 'Play card ' + m.element + '(' + m.rank + ')'
        if (m.location !== null) {
          msg += ' on ' + locToText(m.location)
        }
        break
      default: msg = '(unknwon)'
    }

    dom.append($('<div/>')
               .css('margin-top','1em')
               .css('margin-bottom','0.5em')
               .css('font-weight','bold').text(msg))
  }

  jQuery.each(h, function(ix,m) {
    drawMove(m.move)
    dom.append(drawEventsText(m.events))
  })

  return dom
}

function setSource(x,row,ix,cd) {
  var card = cd.card

  return function() {
    if (helpMode) {
      $('#helpBox').empty()
        .append( $('<h4/>').text(card.name)
               , $('<p>').text(card.description)
               )
      return
    }

    if (!cd.enabled) return

    if (cd.target === 'none') {
      makeTurn('/playCard', { element: row.toLowerCase(), card: ix })
      return
    }

    if (selected !== null) {
      selected.dom.removeClass('selected')
    }
    selected = { dom: x, row: row, ix: ix, card: cd}
    x.addClass('selected')
  }
}

function setTarget(card,who,i) {
  return function(ev) {
    if (selected === null) return
    var matched = false;
    switch(selected.card.target) {

      case 'opponent_creature':
        matched = card !== null && who === 'opponent'
        break

      case 'opponent_normal_creature':
        matched = card !== null && who === 'opponent'
                  && card.element !== 'Special'
        break

      case 'catser_creature':
        matched = card !== null && who === 'caster'
        break

      case 'catser_blank':
        matched = card === null && who === 'caster'
        break

      case 'any_creature':
        matched = card !== null
        break
    }
    if (matched) {
      makeTurn( '/playTargetedCard'
              , { element: selected.row.toLowerCase()
               , card: selected.ix, loc: i, who: who }
              , this
              )
      return
    }
  }
}



function img(x) {
  return $('<img/>')
         .attr('src','img/' + x)
         .css('position','absolute')
         .css('left', '0')
         .css('top', '0')
         .css('width', '96px')
         .css('height','96px')
}

function stat(x,t,l,c) {
  return $('<div/>')
         .css('position','absolute')
         .css('background-color', '#fc9')
         .css('color',c)
         .css('font-size','10px')
         .css('font-weight','bold')
         .css('top',t + 'px')
         .css('left', l + 'px')
         .css('z-index','4')
         .css('padding-left','1px')
         .css('padding-right','1px')
         .css('border-radius','2px')
         .css('border-width', '0px')
         // .css('width','10px')
         .css('height','15px')
         .css('text-align','center')
         .text(x == null ? '?' : x)

}

function drawCardWithExplanation(c) {
  return $('<table/>')
         .append( $('<tr/>')
                  .append($('<td/>').attr('colspan','2').text(c.name))
                , $('<tr/>')
                 .append($('<td/>').append(drawCard(c))
                        ,$('<td/>').text(c.description)))
}

function drawCard(c) {

  var grp = $('<div/>')
            .css('position','relative')
            .css('overflow','hidden')
            .css('display','inline-block')
            .css('width', '96px')
            .css('height','96px')

  if (c == null) {
    // grp.css('border','1px solid #666')
    grp.css('background-color','rgba(0,0,0,0.5)')
       .css('border-radius','5px')
    return grp
  }

  grp.attr('title', c.name + '\n' + c.description)
  if (c.type == 'spell') {
    grp.append(img(c.image)
               .css('height','112')
               .css('top','-16px')
              )
    grp.append(img('ramka2.png')
              .css('z-index','2'))
    grp.append(stat(c.cost,4,80,'black'))

  } else {
    grp.append(img(c.image))
    grp.append(img('ramka1.png')
              .css('z-index','2'))
    grp.append(stat(c.cost,7,76,'black'))
    grp.append(stat(c.life,77,75,'green').addClass('life'))
    grp.append(stat(c.attack,76,10,'red').addClass('attack'))
  }

  return grp
}

function drawDeckCard(c,owner) {
  if (c === null) return drawCard(null)

  var dom = drawCard(c.card).addClass(owner)
  if (!c.enabled) dom.addClass('disabled')

  jQuery.each(c.mods, function(ix,mod) {
    switch (mod.tag) {
      case 'skipAttack': break
      case 'boost':
        dom.find('.attack').append('+' + mod.amount)
        break
      case 'immune': break
    }
  })

  return dom
}

function drawDeckRow(p,row,who) {
  var dom = $('<tr/>')
  var pow = $('<td/>')
            .attr('id',who + '_' + row)

  var val = $('<span/>').addClass('power').text(p.power[row])
  stats[who][row] = p.power[row]
  pow.append($('<span/>').text(row + ': '), val)
  dom.append(pow)
  jQuery.each(p.deck[row], function(ix,card) {
    var x = drawDeckCard(card,who)
            .addClass('deck')
    // if (card.enabled) x.click(setSource(x,row,ix,card))
    x.click(setSource(x,row,ix,card))
    dom.append($('<td/>').append(x))
  })
  return dom
}

function changeWizLife(r,x) {
  stats[r].life += x
  $('#' + r + " .life").text(stats[r].life)
}

function changeWizPow(r,el,x) {
  stats[r][el] += x
  $('#' + r + '_' + el).find('.power').text(stats[r][el])
}

function changeSlotLife(r,l,x) {
  stats[r][l] += x
  $('#' + r + '_' + l).find('.life').text(stats[r][l])
}

function drawPlayer(p,r,winner) {
  var dom = $('<div/>')
            .attr('id',r)
            .css('background-color','rgba(0,0,0,0.5)')
            .css('color','white')
            .css('display','inline-block')
            .css('margin', '20px')
            .css('border-radius','10px')

  var wins = r == winner
  if (wins) dom.css('background-image','url("img/winner-bg.gif")')

  var pref = wins? "Winner: " : (r === 'caster' ? '> ' : '')

  var lifeBox = $('<span/>').addClass('life').text(p.life)
  stats[r].life = p.life

  var name = $('<h3/>')
              .addClass('name')
             .css('text-align','center')
             .append($('<span/>').text(pref + p.name + ' (')
                    , lifeBox
                    ,$('<span/>').text(')')
                    )

  if (wins) name.css('background-color','rgba(0,0,0,0.8)')

  dom.append(name)

  var deckTable = $('<table/>')
  jQuery.each(["Fire","Water","Air","Earth","Special"], function(ix,w) {
    deckTable.append(drawDeckRow(p,w,r))
  })

  dom.append(deckTable)

  return dom
}

function drawArenaField(act,r,i,al) {
  var c = drawDeckCard(act,r).attr('id', r + '_' + i)
                            .addClass(al)
                            .click(setTarget(act, r, i))
  if (act !== null) stats[r][i] = act.card.life
  return c
}

function drawArena(p1,p2,r1,r2) {
  var dom = $('<table/>').css('display','inline-block')
                         .css('valign','bottom')
                         .css('background-color','rgba(0,0,0,0.5)')
  var act1 = p1.active
  var act2 = p2.active
  jQuery.each([0,1,2,3,4,5], function(i,num) {
    var row = $('<tr/>')
    var actL = act1[i]
    var actR = act2[i]
    var x = drawArenaField(actL,r1,i, 'arena_left')
    var y = drawArenaField(actR,r2,i, 'arena_right')
    row.append($('<td/>').append(x),$('<td/>').append(y))
    dom.append(row)
  })
  return dom
}

function drawGame(g,history,winner) {
  var left = g.current
  var right = g.other
  var leftR = 'caster'
  var rightR = 'opponent'

  if (g.left === 'opponent') {
    left = g.other
    right = g.current
    leftR = 'opponent'
    rightR = 'caster'
  }

  var h = drawHistory(history)

  return $('<div/>').append
        ( h
        , $('<table/>')
         .css('margin-left','auto')
         .css('margin-right','auto')
         .append($('<tr/>')
                .append( $('<td/>').append(drawPlayer(left,leftR,winner))
                       , $('<td/>').append(drawArena(left,right,leftR,rightR))
                       , $('<td/>').append(drawPlayer(right, rightR,winner))
                       ))
          )
}


function fancyBtn(txt) {
  return $('<div/>')
         .css('display','inline-block')
         .css('margin', '5px')
         .css('padding','3px')
         .css('color','black')
         .css('background-color','orange')
         .css('border', '1px solid #fc3')
         .css('border-radius', '5px')
         .css('box-shadow', '0px 0px 20px #999')
         .css('cursor','pointer')
         .text(txt)
}


