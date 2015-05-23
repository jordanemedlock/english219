$.redrawCanvas = function() {
  Array.prototype.randomElement = function() {
    return this[Math.floor(Math.random() * this.length)];
  }

  var fillArray = function(value,n) {
    var arr = new Array(n);
    for (var i=0; i < n; i++) {
      arr[i] = value;
    }
    return arr;
  }


  var purpleCard = fillArray("#503e7e",40)
           .concat(fillArray("#a7b2b3",25))
           .concat(fillArray("#958bb2",17))
           .concat(fillArray("#ba263c",5))
           .concat(fillArray("#666947",4))
           .concat(fillArray("#38261d",4))
           .concat(fillArray("#bb9255",3))
           .concat(fillArray("#952d52",1))
           .concat(fillArray("#69ada2",1))
           .concat(fillArray("#985c5a",1));

  var redCard = fillArray("#ca364d",37)
        .concat(fillArray("#77659c",18))
        .concat(fillArray("#7a8395",14))
        .concat(fillArray("#8a1962",11))
        .concat(fillArray("#4c2f2f",8))
        .concat(fillArray("#975173",4))
        .concat(fillArray("#61459a",3))
        .concat(fillArray("#f8b37c",2))
        .concat(fillArray("#e81e26",2))
        .concat(fillArray("#d3817e",2));

  var yellowCard =  fillArray("#ddbb29",17)
            .concat(fillArray("#df6e1f",16))
            .concat(fillArray("#e73923",13))
            .concat(fillArray("#975832",11))
            .concat(fillArray("#8c9d70",11))
            .concat(fillArray("#748c9f",9))
            .concat(fillArray("#49b1b4",9))
            .concat(fillArray("#846861",6))
            .concat(fillArray("#f9faf4",5))
            .concat(fillArray("#ae295e",3));

  var greenCard = fillArray("#3c8e53",20)
          .concat(fillArray("#44877e",10))
          .concat(fillArray("#81be43",10))
          .concat(fillArray("#b9d81c",8))
          .concat(fillArray("#bcce93",5))
          .concat(fillArray("#46532f",5))
          .concat(fillArray("#b79540",4))
          .concat(fillArray("#3f88b6",3))
          .concat(fillArray("#911b1f",1))
          .concat(fillArray("#f8fbfb",1));

  var blueCard =  fillArray("#6769a9",21)
          .concat(fillArray("#c4cdcc",17))
          .concat(fillArray("#a1afce",15))
          .concat(fillArray("#607482",11))
          .concat(fillArray("#3a3893",10))
          .concat(fillArray("#4a312d",7))
          .concat(fillArray("#8ad2dd",7))
          .concat(fillArray("#7196d1",5))
          .concat(fillArray("#b58ecd",4))
          .concat(fillArray("#23244a",2));

  var colorPalates = [purpleCard,redCard,yellowCard,greenCard,blueCard];
  var palate = colorPalates.randomElement();

  $("canvas.square-backgrounds").each(function() {
    var context = this.getContext("2d");
    var canvasWidth = $(this).width();
    var canvasHeight = $(this).height();

    var blockWidth = 30;
    for (var i = 0; i < canvasWidth; i += blockWidth) {
      for (var j = 0; j < canvasHeight; j += blockWidth) {
        var color = palate.randomElement();
        context.fillStyle = color;
        context.fillRect(i,j,blockWidth,blockWidth);
      }
    }
  });
  
}

$(function() {
  $.redrawCanvas();
})