var selected = null

function makeTurn(url,info) {
  jQuery.post(url, info, function(g) {
    $('body').fadeOut('slow').empty().append(drawGame(g)).fadeIn()
  })
}

function setSource(x,row,ix,cd) {

  return function() {
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
    console.log(selected.card.target,matched)
    if (matched) {
      makeTurn( '/playTargetedCard'
              , { element: selected.row.toLowerCase()
               , card: selected.ix, loc: i, who: who })
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
         .css('color',c)
         .css('font-size','10px')
         .css('font-weight','bold')
         .css('top',t + 'px')
         .css('left', l + 'px')
         .css('z-index','4')
         .css('width','10px')
         .css('height','16px')
         .css('text-align','center')
         .text(x == null ? '?' : x)

}

function drawCard(c) {

  var grp = $('<div/>')
            .css('position','relative')
            .css('overflow','hidden')
            .css('display','inline-block')
            .css('width', '96px')
            .css('height','96px')

  if (c == null) {
    grp.css('border','1px solid black')
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
    grp.append(stat(c.life,77,76,'green'))
    grp.append(stat(c.attack,76,10,'red'))
  }

  return grp
}

function drawDeckCard(c) {
  if (c === null) return drawCard(null)

  var dom = drawCard(c.card)
  if (!c.enabled) dom.addClass('disabled')
  return dom
}

function drawDeckRow(p,row) {
  var dom = $('<tr/>')
  var pow = $('<td/>').text(row + ': ' + p.power[row])
  dom.append(pow)
  jQuery.each(p.deck[row], function(ix,card) {
    var x = drawDeckCard(card)
    if (card.enabled) x.click(setSource(x,row,ix,card))
    dom.append($('<td/>').append(x))
  })
  return dom
}

function drawPlayer(p) {
  var dom = $('<div/>')
            .css('display','inline-block')
            .css('margin', '20px')
  var name = $('<h3/>')
             .css('text-align','center')
             .text(p.name + ' (' + p.life + ')')
  dom.append(name)

  var deckTable = $('<table/>')
  jQuery.each(["Fire","Water","Air","Earth","Special"], function(ix,w) {
    deckTable.append(drawDeckRow(p,w))
  })

  dom.append(deckTable)

  return dom
}


function drawArena(p1,p2) {
  var dom = $('<table/>').css('display','inline-block')
                         .css('valign','bottom')
  var act1 = p1.active
  var act2 = p2.active
  jQuery.each([0,1,2,3,4,5], function(i,num) {
    var row = $('<tr/>')
    var actL = act1[i]
    var actR = act2[i]
    var x = drawDeckCard(actL).click(setTarget(actL, 'caster', i))
    var y = drawDeckCard(actR).click(setTarget(actR, 'opponent', i))
    row.append($('<td/>').append(x),$('<td/>').append(y))
    dom.append(row)
  })
  return dom
}

function drawGame(g) {
  return $('<table/>')
         .append($('<tr/>')
                .append( $('<td/>').append(drawPlayer(g.current,true))
                       , $('<td/>').append(drawArena(g.current,g.other))
                       , $('<td/>').append(drawPlayer(g.other,false))
                       ))
}


