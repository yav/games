var poolSelection = null

function redraw(f) {
  $('body').empty().append(drawFactory(f))
}

function call(url,opts) {
  jQuery.post(url,opts,redraw)
}

function mkClick(thing,help,fun) {
  thing.css('cursor','pointer')
       .attr('title',help)
       .click(fun)
}

function testBtn(url,opts) {
  var b = $('<div/>')
          .css('background-color','orange')
          .css('border', '2px solid black')
          .css('display', 'inline-block')
          .css('margin', '4px')
          .css('padding', '4px')
          .text(url)
  mkClick(b,'just for testing', function() { call(url,opts) })
  return b
}

function drawFactory(f) {
  var d = $('<div/>')
  console.log(f)

  d.append(testBtn('/endPeriod',{}))
  d.append(testBtn('/restock',{}))
  d.append(testBtn('/reset',{}))

  d.append(drawPool(f))

  jQuery.each(f.groups, function(ix,g) {
    d.append(drawRuleGroup(ix,g))
  })
  if (f.groupLimit !== null) {
    for(var i = f.groups.length; i < f.groupLimit; ++i) {
      var blank = $('<div/>')
                  .css('background-color', '#ccc')
      d.append(blank)
    }
  }


  d.append(drawOutputs(f.produced))
  d.append(drawDiscarded(f.discarded))

  return d
}

function drawDiscarded(xs) {
  var d = $('<div/>')
  jQuery.each(xs, function(ix,m) {
    d.append(drawMaterial(m))
  })
  return d
}

function drawPool(f) {
  var d = $('<div/>')

  jQuery.each(f.pool, function(ix,a) {
    var m = drawMaterial(a)
    mkClick(m, "Click to select this material.",
      function () {
        if (poolSelection !== null) {
          poolSelection.dom.css('border-width', '2px')
        }
        poolSelection = { dom:m, material: ix }
        poolSelection.dom.css('border-width', '4px')
      })

    d.append(m)
  })

  var i
  for (i = f.pool.length; i < f.poolSize; ++i) {
    d.append(drawMaterial(''))
  }

  d.append($('<div/>')
           .css('font-size','smaller')
           .css('font-style','italic')
           .css('display','inline-block')
           .text('Remaining: ' + f.source +  '/' + f.sourceSize))

  return d
}


function drawRuleGroup(groupId,g) {
  var d = $('<div/>')
          .css('border', '2px solid black')
          .css('margin', '5px')

  jQuery.each(g.rules,function(ix,r) {
    var dom = drawRule(g.activated,groupId,ix,r)
   d.append(dom)
  })

  return d

}

function drawRule(groupIsActive, groupId,ruleId,x) {
  var isActive = x.fired !== undefined
  var bg = isActive ? '#fff' : '#ddd'
  var fg = isActive ? '#cff' : '#ccc'

  var r = $('<div/>')
          .css('background-color', bg)
          .css('margin', '5px')

  var t = $('<div/>')
          .css('background-color', fg)
          .css('margin', '2px')
          .css('font-size','smaller')
          .css('font-style','italic')
          .css('padding', '0 5 0 2')
          .text(x.name)
  console.log(x)
  var isLongTerm = x.produce.tag == 'long_term'

  if (isLongTerm) {
    var inf = $('<b/>')
              .css('float', 'right')
              .html('&#x221e;')
              .attr('title','This item persists across resets.')

    t.append(inf)

    if (x.have !== undefined) {

      var recycleHelp
      var val
      if (x.willreset) {
        recycleHelp = 'This item will be recycled!  Click to undo.'
        inf.css('color', '#ccc')
      } else {
        recycleHelp = 'Click to mark this item for recycling on next reset.'
      }
      mkClick(inf, recycleHelp
             , function() { call('/setReset',
                              { group: groupId, value: !x.willreset} ) })
    }
  }

  var row_num = isLongTerm ? 1 : x.produce.outputs.length
  var b = $('<table/>')
  var have = $('<td/>')
             .attr('rowspan', row_num)
  if (x.have !== undefined)
    have.append(drawInputs(x.have))

  var tr = $('<tr/>')
           .append([ $('<td/>').append(drawInputs(x.inputs))
                               .attr('rowspan', row_num)
                   , $('<td/>').html('&#x2192')
                     .attr('rowspan', row_num)
                   , have
                   ])

  var ready = x.inputs.length === 0 && !x.fired
  if (isActive && !x.fired && !ready || !groupIsActive) {
    mkClick(r, 'Click to add material.',
      function() {
        if (poolSelection === null) {
          console.log('Need a selection.')
          return
        } else {
          call('/apply', { group: groupId
                         , rule: ruleId
                         , material: poolSelection.material })
        }
     })
  }

  if (isLongTerm) {
    console.log('XXX: Long term')
  } else {

    jQuery.each(x.produce.outputs,function(ix,n) {
      var td = $('<td/>')
               .append(drawOutputs(n))
      if (ready) {
        td.css('background-color', '#ff0')
        mkClick(td, "Click to collect actions."
               , function() { call('/produce'
                            , { group: groupId, variant: ix }) })
      }

      if (ix + 1 < row_num)
        td.css('border-bottom','1px solid #ccc')
      tr.append(td)
      b.append(tr)
      tr = $('<tr/>')
    })
  }


  r.append([t,b])
  return r
}

function drawOutputs(x) {
  var d = $('<div/>')
          .css('display','inline-block')
          .css('padding', '4px')
  var first = true
  jQuery.each(x,function(t,n) {
    if (n === 0) return true
    var a = $('<div/>')
            .css('display','inline-block')
    if (n > 1) {
      a.append($('<div/>')
               .html(n + '&times;')
               .css('display','inline-block')
              )
    }
    a.append(drawAction(t))
    if (!first)
      a.css('margin-left', '1em')

    d.append(a)
    first = false
  })
  return d
}

function drawAction(x) {
  return $('<div/>')
         .text(x)
         .css('display','inline-block')
}

function drawInputs(x) {
  var d = $('<div/>')
          .css('display','inline-block')
  jQuery.each(x,function(ix,m) {
    d.append(drawMaterial(m))
  })
  return d
}

function drawMaterial(x) {
  var c;
  var colors = [ '#f00', '#0f0', '#00f', '#ff0', '#f0f', '#0ff' ]

  switch(x) {
    case 'A':     c = colors[0]; break
    case 'B':     c = colors[1]; break
    case 'C':     c = colors[2]; break
    case 'D':     c = colors[3]; break
    case 'E':     c = colors[4]; break
    case 'F':     c = colors[5]; break
    case 'waste': c = '#999';    break
  }

  var m = $('<div/>')
          .css('width',            '15px')
          .css('height',           '16px')
          .css('margin',           '2px')
          .css('line-height',      '0px')
          .css('display',          'inline-block')
          .css('vertical-align',   'middle')

  if (x === 'wild') {
    m.css('background-color', c)
     .css('border',           '2px solid #000')
     .css('position',         'relative')
    var smallW = 15/3
    var smallH = 16/2
    var i, j, col = 0
    for (i = 0; i < 3; i++)
      for (j = 0; j < 2; j++)
        m.append($('<div/>')
                 .css('position', 'absolute')
                 .css('width',    smallW + 'px')
                 .css('height',   smallH + 'px')
                 .css('line-height', '0px')
                 .css('left', i * smallW + 'px')
                 .css('top',  j * smallH + 'px')
                 .css('background-color', colors[col++])
                 )

  } else
  if (c !== undefined) {
    m.css('background-color', c)
     .css('border',           '2px solid #000')
  } else {
    m.css('background-color', 'rgba(0,0,0,0.2)')
     .css('border',           '2px solid #eee')
  }

  return m
}


