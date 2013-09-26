var Scraper = {};
var GameState = {};

Scraper.ring = [];

GameState.modularity = 0;
GameState.board = { dimensions : [], data : [] };
GameState.shapes = [];

Scraper.getBoard = function () {
  var rows = $( ".content > table tr" ).length;
  var cells = $( ".content > table a" ).length;
  GameState.board.dimensions = [rows, cells/rows];
  $( ".content > table a img" ).each( function(index, Element) {
    var reg = /[^\/]*_/;
    var match = reg.exec( Element.src );
    if ( match !== null ) {
      GameState.board.data.push( Scraper.ring.indexOf( match[0] ) );
    }
  });
};

Scraper.getModularity = function () {
  $( "#content .content center table img" ).each( function(index, Element) {
    var reg = /[^\/]*_/;
    var match = reg.exec( Element.src );
    if ( match !== null ) {
      Scraper.ring.push( match[0] );
    }
  });
  Scraper.ring.pop();
  Scraper.ring.unshift( Scraper.ring.pop() );
  GameState.modularity = Scraper.ring.length;
};

Scraper.getShapes = function () {
  $( "img[height=10][width=10]" ).closest("table").each( function(index, Element) {
    var cells = $(this).find( "td" );
    var rows = $(this).find( "tr" ).length;
    var data = [];
    cells.each( function(index, Element) {
      data.push( ($(this).html() === "") ? 0 : 1 );
    });
    GameState.shapes.push( { dimensions : [rows, (cells.length)/rows], data : data } );
  });
};

Scraper.scrape = function () {
  Scraper.getModularity();
  Scraper.getBoard();
  Scraper.getShapes();
};
