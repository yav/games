function redrawGame(g) {
  $('#content').empty().append(drawGame(g))
}

function drawGame(g) {
  var mainFrame = $('<div/>').addClass('mainFrame')
  var left      = $('<div/>').addClass('left')
  var middle    = $('<div/>').addClass('middle')
  var right     = $('<div/>').addClass('right')


  var game = g.game
  var r    = game.resources

  if (g.question !== null) {
    middle.append(drawQuestion(g.question))
  }



  mainFrame.append([left,middle,right])

  left.append(drawMap(game.land))
  left.append(drawTrove(r.trove))

  middle.append([ drawPoints(r), drawNext() ])

  right.append(drawActions(r.actions))

  return mainFrame
}

function drawNext() {
  var me = $('<div/>').text('Nex')
           .addClass('clickable')
           .click(function() {
              jQuery.post('/nextTurn',{}, redrawGame)
           })
  return me
}


function drawResourceAmount(el,amt) {

  var me = $('<div/>')
           .addClass('field')
           .append( el )

  if (amt !== 1) {
    me.append(  $('<div/>')
                .addClass('value')
                .append( [ $('<span/>').text(amt)
                         , $('<div/>').addClass('strut')
                         ] ))
  }

  return me
}


function drawPoints(r) {
  var me = $('<div/>').addClass('points bordered')

  jQuery.each(r.plane.summoned, function(ix,s) {
    me.append(drawResourceAmount(drawSpirit(s.type), s.amount))
  })

  jQuery.each(r.points, function(ix,e) {
    var img = $('<div/>').addClass('item')
                         .addClass(e.type)
                         .html('&nbsp;')
    me.append(drawResourceAmount(img,e.amount))
  })

  jQuery.each(r.plane.friendly, function(ix,e) {
    var it = drawStoredSpirit(e.type,64)
              .addClass('clickable')
              .click(function() {
                jQuery.post('/summonStored', { spirit: e.type }, redrawGame)
               })
    me.append(drawResourceAmount(it,e.amount))
  })

  return me
}


function drawActions(as) {

  var me  = $('<div/>').addClass('actions')
  var act = $('<div/>').addClass('active')
  var us  = $('<div/>').addClass('used')

  jQuery.each (as.available, function(ix,a) {
    act.append(drawAction(a,true))
  })
  jQuery.each(as.used, function(ix,a) {
    us.append(drawAction(a,false))
  })
  me.append([act,us])
  return me
}

function drawSpirit(x) {
  return $('<div/>')
         .addClass('item')
         .addClass(x)
         .html('&nbsp;')
}

function drawStoredSpirit(x,sz) {
  function px(x) { return x + 'px' }
  var smaller = Math.round(0.75 * sz)
  var me = $('<div/>')
           .css('background-image', 'url("img/' + x + '.svg")')
           .css('background-repeat', 'no-repeat')
           .css('background-position', 'center')
           .css('background-size', px(smaller) + ' ' + px(smaller))
           .css('width', px(sz))
           .css('height', px(sz))
           .append($('<img/>')
                   .attr('src', 'img/container_transparent.svg')
                   .css('width',  px(sz))
                   .css('height', px(sz))
                   )

  return me

}


function drawQuestion(q) {
  var me = $('<div/>').addClass('question bordered')

  var things = {}
  jQuery.each(q, function(ix,opt) {
    var d = drawDescription(opt)
    d.addClass('clickable')
     .click(function() { jQuery.post('/answer', { answer: ix }, redrawGame) })
    me.append(d)
  })

  return me
}



function drawTrove(t) {
  var me = $('<div/>').addClass('trove bordered')

  var how_many = drawResourceAmount
                  ($('<div/>').addClass('item well'), t.summon)

  me.append(how_many)

  jQuery.each(t.spirits, function(ix,s) {



    var help = ''
    switch (s.state) {
      case 'available':
        amt = 1
        switch (s.spirit) {
          case 'death': help = 'This spirit has been lost.'; break
          case 'birth': help = 'You may choose what element to summon.'; break
          default: help = 'You may summon this spirit.'; break
        }
        break
      case 'summoned':
        amt = 'x'
        help = 'A new spirit will be available on the next turn.'; break
      case 'transformed':
        amt = '-'
        help = 'This spirit will be available on the next turn.'; break
    }

    var it = drawResourceAmount(drawSpirit(s.spirit), amt)
            .attr('title',help)

    if (s.state === 'available' && s.spirit !== 'death') {
      it.addClass('clickable')
      it.click(function() {
        jQuery.post('/summon', { 'spirit': s.spirit }, redrawGame)
      })
    }
    me.append(it)
  })

  return me
}


function drawAction(a, clickable) {

  var top    = $('<td/>').addClass('small top')
  var bottom = $('<td/>').addClass('small bottom')

  function makeIt(where,d) {
    var here = where === 'basic' ? top : bottom
    var it = $('<div/>').append(drawDescription(d))
    if (clickable) {
        it.addClass('clickable')
          .click(function() {
            jQuery.post('/action', { action: a.name
                                   , type: where
                                   }, redrawGame)
          })
    }
    here.append(it)
  }

  makeIt('basic', a.basic)
  makeIt('advanced', a.advanced)

  return $('<div/>').addClass('box')
         .append($('<table/>').addClass(a.element)
                 .append([ $('<tr/>').append($('<th/>').text(a.name))
                         , $('<tr/>').append(top)
                         , $('<tr/>').append(drawSpirit(a.element)
                                             .addClass('icon'))
                         , $('<tr/>').append(bottom)
                         ])
                )
}

function drawDescription(d0) {

  var cur = $('<div/>').addClass('text-layout')
  go(d0)
  return cur

  function go(d) {

    switch(d.tag) {
      case 'text':
        return cur.append($('<span/>').text(d.val))

      case 'image':
        return cur.append($('<img/>').addClass('icon')
                          .attr('src','img/' + d.val + '.svg'))

      case 'action':
        return cur.append(drawAction(d.val,false))

      case 'hor':
        return cur
               .append( [ go(d.left)
                        , go(d.right)
                       ] )

      case 'ver':
        var save = cur
        var top = $('<div/>')
        var bot = $('<div/>')
        cur = top
        go(d.above)
        cur = bot
        go(d.below)
        cur = save
        cur.append($('<div/>').addClass('text-layout')
                   .append([top,bot]))
    }
  }
}
