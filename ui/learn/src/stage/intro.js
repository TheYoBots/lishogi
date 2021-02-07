var util = require("../util");
const assert = require("../assert");
var arrow = util.arrow;

module.exports = {
  key: "intoduction",
  title: "theIntoduction",
  subtitle: "introBasics",
  image: util.assetUrl + "images/learn/pieces/I.svg",
  intro: "introIntro",
  illustration: util.pieceImg("dragon"),
  levels: [
    {
      goal: "choosePieceDesign",
      fen: "9/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w -",
      nbMoves: 0,
      text: "Click here after you've chosen the piece design you like.",
      end: true,
    },
    {
      goal: "promotionZone",
      fen: "9/9/9/4P4/9/9/9/9/9 w -",
      nbMoves: 1,
      shapes: [arrow("e6e7", "blue")],
      lastMoves: [
        "a9",
        "b9",
        "c9",
        "d9",
        "e9",
        "f9",
        "g9",
        "h9",
        "i9",
        "a8",
        "b8",
        "c8",
        "d8",
        "e8",
        "f8",
        "g8",
        "h8",
        "i8",
        "a7",
        "b7",
        "c7",
        "d7",
        "e7",
        "f7",
        "g7",
        "h7",
        "i7",
      ],
    },
    {
      goal: "senteGoesFirst",
      fen: "9/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w -",
      nbMoves: 1,
    },
  ].map(util.toLevel),
  complete: "introComplete",
};