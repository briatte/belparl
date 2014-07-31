<?php

  if(count($_GET) > 0) {
    if(!empty($_GET['legislature'])) $page = basename($_GET['legislature']);
    if(!empty($_GET['chamber'])) $ch = basename($_GET['chamber']);
  }

  if(!isset($page)) $page = "48";
  if($page == '') $page = "48";

  if(!isset($ch)) $ch = "ch";
  if($ch == '') $ch = "ch";

  if($ch == "ch")
    $chamber = "Chambre";
  else
    $chamber = "Sénat";
  
  $array = array(
    "54" => "2014&mdash;",
    "53" => "2010&mdash;2014",
    "52" => "2007&mdash;2010",
    "51" => "2003&mdash;2007",
    "50" => "1999&mdash;2003",
    "49" => "1995&mdash;1999",
    "48" => "1991&mdash;1995",
    "47" => "1988&mdash;1991");
  $class = array(
    "54" => "",
    "53" => "",
    "52" => "",
    "51" => "",
    "50" => "",
    "49" => "",
    "48" => "",
    "47" => "");
  $class[$page] = "here";

  // ongoing legislature
  $be = "was";
  if($page == '54') $be = "is";

  $have = "had";
  if($page == '54') $have = "has had";
  
  // initial caption
  if($ch == "ch")
    $caption = '<p>This graph shows Belgian Members of Parliament (MPs) during the ' . $page . 'th&nbsp;legislature. A link between two MPs indicates that they have cosponsored at least one bill.';
  else
    $caption = '<p>This graph shows Belgian Senators during the ' . $page . 'th&nbsp;legislature. A link between two Senators indicates that they have cosponsored at least one bill.';

  $caption = $caption . ' Their size is proportional to their <a href="http://toreopsahl.com/tnet/weighted-networks/node-centrality/">weighted degree</a>. See <a href="plots.html">this page</a> for more plots.</p>'
?>

<!doctype html>
<html>
<head>
  <title><?php
    echo "Cosponsorship networks in the Belgian Parliament: ";
    if($ch == "ch")
      echo "Chambre, législature ";
    else
      echo "Sénat, législature ";
    echo $page;
    ?>
  </title>
  <meta charset="utf-8">
  <link href="http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,600" rel="stylesheet" type="text/css" />
  <link href="/assets/styles.css" rel="stylesheet" type="text/css" />
  <link rel="stylesheet" href="/assets/font-awesome-4.0.3/css/font-awesome.min.css">
  <style type="text/css" media="screen">
  html, body {
    font: 24px/150% "Source Sans Pro", sans-serif;
    /* background-image: url("hemicycle_<?php echo $ch; ?>.jpg"); */
    background-color: #333;
    color: #fff;
    margin: 0;
    padding:0;
    width: 100%;
    height: 100%;
  }
  </style>
</head>
<body>

<div id="sigma-container">
  <div id="controls" class="bg_gr">
    <!-- <?php if($ch == "se") echo $ch; else echo "an"; ?> -->
    <h1>cosponsorship networks</h1>    
    <h2><a href="<?php if($ch=="ch") echo "http://www.lachambre.be/"; else echo "http://www.senate.be/"; ?>" title="<?php echo $chamber; ?>">
           <img src="logo_<?php echo $ch; ?>.png" height="25" alt="logo">
        </a>&nbsp;<?php echo $chamber . ", " . $array[ $page ]; ?></h2>

    <p>Chamber&nbsp;&nbsp;
      <?php

      if($ch == "ch")
        echo "<a href='?chamber=ch&amp;legislature=$page' class='here'>Lower</a>";
      else
        echo "<a href='?chamber=ch&amp;legislature=$page'>Lower</a>";
      echo "&nbsp;&nbsp;";

      if($ch == "se")
        echo "<a href='?chamber=se&amp;legislature=$page' class='here'>Upper</a>";
      else if($page == "48")
        echo "<a href='?chamber=se&amp;legislature=49'>Upper (starts in 1995)</a>";
      else
        echo "<a href='?chamber=se&amp;legislature=$page'>Upper</a>";
      ?>
      <br>
      Legislature&nbsp;&nbsp;
      <?php
      if($ch == "ch")
        echo'<a href="?chamber=' . $ch . '&amp;legislature=48" class=' . $class["48"] . '>1991&mdash;1995</a>&nbsp;&nbsp;';
      ?>
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=49" class='<?php echo $class["49"]; ?>'>1995&mdash;1999</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=50" class='<?php echo $class["50"]; ?>'>1999&mdash;2003</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=51" class='<?php echo $class["51"]; ?>'>2003&mdash;2007</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=52" class='<?php echo $class["52"]; ?>'>2007&mdash;2010</a>&nbsp;&nbsp;
      <a href="?chamber=<?php echo $ch; ?>&amp;legislature=53" class='<?php echo $class["53"]; ?>'>2010&mdash;2014</a>&nbsp;&nbsp;
      <!-- <a href="?chamber=<?php echo $ch; ?>&amp;legislature=54" class='<?php echo $class["54"]; ?>'>2014&mdash;</a> -->
      <!-- <label title="Map MPs to their constituencies (very approximative).">
        &nbsp;<input type="checkbox" id="showMap" />
        Map
      </label> -->
    </p>
    
    <!-- user search field -->
    <form action="/" method="post" class="search-nodes-form">
      <fieldset id="search-nodes-fieldset">
        <div></div>
      </fieldset>
    </form>

    <p>
      Click a node to show its ego network. Double click to zoom in or out.<br>
      Hide&nbsp;
      <label title="Do not draw network ties (vertex edges).">
        <input type="checkbox" id="showEdges" />
        Edges
      </label>
      &nbsp;
      <label title="Do not add labels to nodes (MP names) when zooming in.">
        <input type="checkbox" id="showLabels" />
        Labels
      </label>
      &nbsp;
      <label title="Draw only ties formed among frequent cosponsors (edge weight > 0.5).">
        <input type="checkbox" id="showSparse" />
        Weak ties
      </label>
      <br>
      Download&nbsp;&nbsp;<i class="fa fa-file-o"></i>&nbsp;&nbsp;<a href="<?php echo 'net_' . $ch . $page; ?>.gexf" title="Download this graph (GEXF, readable with Gephi)">network</a>&nbsp;&nbsp;<i class="fa fa-files-o"></i>&nbsp;&nbsp;<a href="<?php echo $ch; ?>.zip" title="Download all <?php echo $chamber; ?> graphs (GEXF, readable with Gephi)">full series</a></p>
    <p><a href="#" id="recenter-camera" class="button" title="Reset graph to initial zoom position.">reset zoom</a>&nbsp;&nbsp;<a href="#" id="toggle-layout" class="button" title="Animate with Force Atlas 2.">Animate</a> <small><a href="https://gephi.org/2011/forceatlas2-the-new-version-of-our-home-brew-layout/" title="Details on the Force Atlas 2 algorithm."><i class="fa fa-info-circle"></i></a></small></p>
    <footer>
      <p>Inspired by <a href="http://coulmont.com/blog/2011/09/02/travail-de-deputes/">Baptiste&nbsp;Coulmont</a> and <a href="http://jhfowler.ucsd.edu/cosponsorship.htm">James&nbsp;Fowler</a>, built with <a href="http://gexf.net/format/" title="GEXF file format (Gephi)">GEXF</a>, <a href="http://www.r-project.org/" title="The R Project for Statistical Computing">R</a> and <a href="http://sigmajs.org/" title"JavaScript library dedicated to graph drawing">sigma.js</a>. 
      <!-- Background photo by <?php if($ch == "ch") echo "<a href='http://commons.wikimedia.org/wiki/File:Panorama_de_l%27h%C3%A9micyle_de_l%27assembl%C3%A9e_nationale.jpg' title='Original photograph by Richard Ying and Tangui Morlier'>Richard Ying and Tangui Morlier"; else echo "<a href='https://commons.wikimedia.org/wiki/File:L%27h%C3%A9micycle_du_S%C3%A9nat_fran%C3%A7ais_en_septembre_2009.jpg' title='Original photograph by Romain Vincens'>Romain Vincens"; ?></a> (Wikimedia). --></p>
      <p><a href="http://twitter.com/share?text=Cosponsorship%20networks%20in%20the%20Belgian%20Parliament,%20by%20@phnk:&amp;url=<?php echo 'http://' . $_SERVER["SERVER_NAME"].$_SERVER["REQUEST_URI"]; ?>" class="button" title="Share this page on Twitter."><i class="fa fa-twitter"></i> Tweet</a>&nbsp;&nbsp;<a href="https://github.com/briatte/belparl" class="button" title="Get the code and data from GitHub."><i class="fa fa-github"></i> Code</a></p>
    </footer>
    <div id="graph-container"></div>
  </div>
  <div id="caption" class="bg_gr">
    <?php echo $caption; ?>
  </div>

</div>

<script type="text/javascript" src="/assets/jquery.min.js"></script>
<script type="text/javascript" src="/assets/jquery.smart_autocomplete.min.js"></script>
<script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/sigma.min.js"></script>
<script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/plugins/sigma.parsers.gexf.min.js"></script>
<script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/plugins/sigma.layout.forceAtlas2.min.js"></script>

<script>
function decimalAdjust(type, value, exp) {
	// If the exp is undefined or zero...
	if (typeof exp === 'undefined' || +exp === 0) {
		return Math[type](value);
	}
	value = +value;
	exp = +exp;
	// If the value is not a number or the exp is not an integer...
	if (isNaN(value) || !(typeof exp === 'number' && exp % 1 === 0)) {
		return NaN;
	}
	// Shift
	value = value.toString().split('e');
	value = Math[type](+(value[0] + 'e' + (value[1] ? (+value[1] - exp) : -exp)));
	// Shift back
	value = value.toString().split('e');
	return +(value[0] + 'e' + (value[1] ? (+value[1] + exp) : exp));
}

// Decimal round
if (!Math.round10) {
	Math.round10 = function(value, exp) {
		return decimalAdjust('round', value, exp);
	};
}

// Add a method to the graph model that returns an
// object with every neighbors of a node inside:
sigma.classes.graph.addMethod('neighbors', function(nodeId) {
  var k,
      neighbors = {},
      index = this.allNeighborsIndex[nodeId] || {};

  for (k in index)
    neighbors[k] = this.nodesIndex[k];

  return neighbors;
});

sigma.classes.graph.addMethod('getNeighborsCount', function(nodeId) {
  return this.allNeighborsCount[nodeId];
});

sigma.parsers.gexf(
  document.title.replace("Chambre", "net_ch").replace("Sénat", "net_se").replace(", législature ", "").replace("Cosponsorship networks in the Belgian Parliament: ", "")+'.gexf',
  { // Here is the ID of the DOM element that
    // will contain the graph:
    container: 'sigma-container'
  },
  function(s) {
      
    // We first need to save the original colors of our
    // nodes and edges, like this:
    s.graph.nodes().forEach(function(n) {
      n.originalColor = n.color;
      n.originalX = n.x;
      n.originalY = n.y;
    });
    s.graph.edges().forEach(function(e) {
      e.originalColor = e.color;
      e.type = 'arrow';
    });

    // When a node is clicked, we check for each node
    // if it is a neighbor of the clicked one. If not,
    // we set its color as grey, and else, it takes its
    // original color.
    // We do the same for the edges, and we only keep
    // edges that have both extremities colored.
    s.bind('clickNode', function(e) {
      var nodeId = e.data.node.id,
          toKeep = s.graph.neighbors(nodeId);
      toKeep[nodeId] = e.data.node;
      
      s.graph.nodes().forEach(function(n) {
        if (toKeep[n.id])
          n.color = n.originalColor;
        else
          n.color = '#555';
      });

      s.graph.edges().forEach(function(e) {
        if (toKeep[e.source] && toKeep[e.target])
          e.color = e.originalColor;
        else
          e.color = '#333';
      });
            
      // append mandate suffix
      //
      // var mandats = e.data.node.attributes['nb_mandats'];
      // if (mandats == "1")
      //   mandats = "first";
      // else if (mandats == "2")
      //   mandats = "second"
      // else if (mandats == "3")
      //   mandats = "third"
      // else
      //   mandats = mandats + 'th';

      // node color
      var rgba = e.data.node.color;

      // explicit party groups
      group = e.data.node.attributes['party'];
      party = "?";
      if(group == "ECOLO") party = "Greens";
      if(group == "C-DEM-F") party = "Francophone Conservatives";
      if(group == "C-DEM-V") party = "Flemish Conservatives";
      if(group == "C-DEM-V/VOLKS") party = "Flemish Conservatives/Volksunie";
      if(group == "LIB-F") party = "Francophone Liberals";
      if(group == "LIB-V") party = "Flemish Liberals";
      if(group == "SOC-F") party = "Francophone Socialists";
      if(group == "SOC-V") party = "Flemish Socialists";
      if(group == "FN") party = "Front National";
      if(group == "VLAAMS") party = "Vlaams Blok";
      if(group == "VOLKS") party = "Volksunie";
      if(group == "VOLKS") party = "Volksunie";
      if(group == "ROSSEM") party = "ROSSEM";
      if(group == "LDD") party = "<i lang='nl'>Libertair, Direct, Democratisch</i>";
      if(group == "INDEP") party = "independent";

      if(document.title.match('Chambre'))
        profile = "<a href='http://www.lachambre.be/kvvcr/showpage.cfm?section=/depute&language=fr&rightmenu=right_depute&cfm=cvview54.cfm?key=" + e.data.node.attributes['link'] + "' title='Go to profile (<?php echo $chamber; ?>, new window)' target='_blank'>";
      else
        profile = "<a href='http://www.senate.be/www/?MIval=/showSenator&ID=" + e.data.node.attributes['sid'] + "' title='Go to profile (<?php echo $chamber; ?>, new window)' target='_blank'>";

      distance = "around " + Math.round10(e.data.node.attributes['distance'], -1);
      if(isNaN(e.data.node.attributes['distance']))
        distance = "impossible to compute (too isolated)";
      if(document.title.match('Chambre'))
        document.getElementById('caption').innerHTML = '<p style="background:' + rgba + ';">' + profile + '<img class="small" src="' + e.data.node.attributes['photo'].replace("/site/", "http://www.lachambre.be/site/") + '" alt="no photo available" /></a> You selected ' + profile + e.data.node.attributes['name'] + '</a> <span title="Political party affiliation(s): ' + group.replace('/', ", ") + '" style="color:' + rgba.replace('0.3)', '1)') + ';">(' + party + ')</span>, who <?php echo $have; ?> <span title="unweighted Freeman degree">' + s.graph.getNeighborsCount(nodeId) + ' bill cosponsor(s)</span> during the legislature. The <a href="http://toreopsahl.com/tnet/weighted-networks/shortest-paths/">mean weighted distance</a> between this MP and all others <?php echo $be; ?>&nbsp;' + distance + '.</p>';
      else
        document.getElementById('caption').innerHTML = '<p style="background:' + rgba + ';">' + profile + '<img class="small" src="http://www.senate.be/www/?MItabObj=persoon&MIcolObj=foto&MInamObj=persoonid&MIvalObj=' + e.data.node.attributes['sid'] + '&MItypeObj=image/gif" alt="no photo available" /></a> You selected ' + profile + e.data.node.attributes['name'] + '</a> <span title="Political party affiliation(s): ' + group.replace('/', ", ") + '" style="color:' + rgba.replace('0.3)', '1)') + ';">(' + party + ')</span>, who <?php echo $have; ?> <span title="unweighted Freeman degree">' + s.graph.getNeighborsCount(nodeId) + ' bill cosponsor(s)</span> during the legislature. The <a href="http://toreopsahl.com/tnet/weighted-networks/shortest-paths/">mean weighted distance</a> between this MP and all others <?php echo $be; ?>&nbsp;' + distance + '.</p>';
      
      // Since the data has been modified, we need to
      // call the refresh method to make the colors
      // update effective.
      s.refresh();
    });

    // When the stage is clicked, we just color each
    // node and edge with its original color.
    s.bind('clickStage', function(e) {
      s.graph.nodes().forEach(function(n) {
        n.color = n.originalColor;
      });

      s.graph.edges().forEach(function(e) {
        e.color = e.originalColor;
      });

      // Same as in the previous event:
      s.refresh();
      
      document.getElementById('caption').innerHTML = '<?php echo $caption; ?>';
    });
    
    s.settings({
      defaultEdgeColor: '#555',
      edgeColor: 'source',
      minNodeSize: 2,
      maxNodeSize: 6,
      defaultLabelColor: '#fff',
      defaultLabelSize: 18,
      font: "source sans pro",
      minEdgeSize: .1,
      maxEdgeSize: .3,
      labelHoverBGColor: 'node',
      defaultLabelHoverColor: '#fff',
      labelHoverShadow: 'node'
    });
    
    // autocomplete search field
    //
    $('#search-nodes-fieldset > div').remove();
    $('<div>' +
        '<label for="search-nodes">' +
          'Search' +
        '</label>' +
        '<input type="text" autocomplete="off" id="search-nodes"/>' +
      '</div>').appendTo('#search-nodes-fieldset');

    $('#search-nodes-fieldset #search-nodes').smartAutoComplete({
      source: s.graph.nodes().map(function(n){
        return n.label;
      })
    }).bind('itemSelect', function(e) {
      var label = e.smartAutocompleteData.item.innerText;

      // find node and neighbours
      var id = 0,
          nodeId = 0,
          toKeep = new Array();
      s.graph.nodes().forEach(function(n) {
        if (n.label == label) {
          id = n.id;
          nodeId = n.id,
          toKeep = s.graph.neighbors(nodeId);
        }
      });
            
      // color selected nodes
      s.graph.nodes().forEach(function(n) {
        if (n.id == id)
          n.color = n.originalColor;
        else if(toKeep[n.id])
          n.color = '#999';
        else
          n.color = '#555';
      });

      // color selected edges
      s.graph.edges().forEach(function(e) {
        if (toKeep[e.source] && toKeep[e.target])
          e.color = e.originalColor;
        else
          e.color = '#333';
      });

      s.refresh();
      
    });

    // protect search field
    //
    $('form.search-nodes-form').submit(function(e) {
      e.preventDefault();
    });
       
    // show it all, finally
    s.refresh();
    
    // hide edges
    //
    document.getElementById('showEdges').addEventListener('change',
    function(e){
      if (e.target.checked) {
        s.settings({
          drawEdges: false
        });
      } else {
        s.settings({
          drawEdges: true
        });
      }
      s.refresh();
    });
    
    // hide labels
    //
    document.getElementById('showLabels').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.settings({
          drawLabels: false
        });
      } else {
        s.settings({
          drawLabels: true
        });
      }
      s.refresh();
    }); 
    
    // hide sparse ties
    //
    document.getElementById('showSparse').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.graph.edges().forEach(function(e) {
          // use upper quartile marker
          if(e.weight < 2)
            e.color = 'rgba(66,66,66,0)';
        });
        s.settings({
          minEdgeSize: 0,
          maxEdgeSize: .9
        });
      } else {
        s.graph.edges().forEach(function(e) {
          e.color = e.originalColor;
        });
        s.settings({
          minEdgeSize: .1,
          maxEdgeSize: .3,
        });
      }
      s.refresh();
    }); 
        
    // view as map
    //
    // document.getElementById('showMap').addEventListener('change',
    // function(e){
    //   if (e.target.checked) {
    //     s.graph.nodes().forEach(function(n) {
    //
    //       // map node position to lon/lat
    //       n.x = n.attributes['lon'];
    //       n.y = n.attributes['lat'] * -1;
    //
    //     });
    //   } else {
    //     s.graph.nodes().forEach(function(n) {
    //       n.x = n.originalX;
    //       n.y = n.originalY;
    //     });
    //   }
    //   s.refresh();
    // });
    
    // force atlas
    //
    document.getElementById('toggle-layout').addEventListener('click', 
    function() {
      if ((s.forceatlas2 || {}).isRunning) {
        s.stopForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Animate';
      } else {
        s.startForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Stop';
      }
    });
    
    // reset zoom
    //
    document.getElementById('recenter-camera').addEventListener('click',
    function() {
      s.cameras[0].goTo({
                x: 0,
                y: 0,
                angle: 0,
                ratio: 1
              });
    });
    
  }
);

</script>

</body>
</html>
