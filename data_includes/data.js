//
// Define a new "Boxes" controller, which is a combination of a "Message" (the
// boxes) and a "Question" (whether the target box follows the rule).
//
define_ibex_controller({
    name: "Boxes",
    jqueryWidget: {
        _init: function () {
            this.options.transfer = null; // Remove "Click here to continue" message.
            this.element.VBox({
                options: this.options,
                triggers: [1],
                children: [
                    "Message", this.options,
                    "Question", this.options
                ]
            });
        }
    },
    properties: { }
});

var shuffleSequence = seq("intro", "practice", randomize("real"));
var showProgressBar = false;
// var centerItems = false;

var defaults = [
    // "Separator", {
    //     transfer: 1000,
    //     normalMessage: "Please wait for the next item."
    // },
    "Form", {
        continueOnReturn: true,
        saveReactionTime: true
    },
    "Boxes", {
        as: ["The box satisfies the rule.", "The box does not satisfy the rule."],
        hasCorrect: false,
        presentHorizontally: true,
        // autoFirstChar: true
    }
];

var items = [

    ["sep", "Separator", { }],

    //
    // Introductory form for gathering participant information.
    //
    ["intro", "Form", {
        html: {include: "intro.html"}
    }],

    //
    // Four practice trials.
    //
    ["practice", "Boxes", {html: {include: "practice1.html"}}],
    ["practice", "Boxes", {html: {include: "practice2.html"}}],
    ["practice", "Boxes", {html: {include: "practice3.html"}}],
    ["practice", "Boxes", {html: {include: "practice4.html"}}],

    //
    // The real trials.
    //
    ["real", "Boxes", {html: {include: "item1.html"}}],
    ["real", "Boxes", {html: {include: "item2.html"}}],
    ["real", "Boxes", {html: {include: "item3.html"}}],
    ["real", "Boxes", {html: {include: "item4.html"}}],
    ["real", "Boxes", {html: {include: "item5.html"}}],
    ["real", "Boxes", {html: {include: "item6.html"}}],
    ["real", "Boxes", {html: {include: "item7.html"}}],
    ["real", "Boxes", {html: {include: "item8.html"}}],
    ["real", "Boxes", {html: {include: "item9.html"}}],
    ["real", "Boxes", {html: {include: "item10.html"}}],
    ["real", "Boxes", {html: {include: "item11.html"}}],
    ["real", "Boxes", {html: {include: "item12.html"}}],
    ["real", "Boxes", {html: {include: "item13.html"}}],
    ["real", "Boxes", {html: {include: "item14.html"}}],
    ["real", "Boxes", {html: {include: "item15.html"}}],
    ["real", "Boxes", {html: {include: "item16.html"}}],
    ["real", "Boxes", {html: {include: "item17.html"}}],
    ["real", "Boxes", {html: {include: "item18.html"}}],
    ["real", "Boxes", {html: {include: "item19.html"}}],
    ["real", "Boxes", {html: {include: "item20.html"}}],
    ["real", "Boxes", {html: {include: "item21.html"}}],
    ["real", "Boxes", {html: {include: "item22.html"}}],
    ["real", "Boxes", {html: {include: "item23.html"}}],
    ["real", "Boxes", {html: {include: "item24.html"}}],
    ["real", "Boxes", {html: {include: "item25.html"}}],
    ["real", "Boxes", {html: {include: "item26.html"}}],
    ["real", "Boxes", {html: {include: "item27.html"}}],
    ["real", "Boxes", {html: {include: "item28.html"}}],
    ["real", "Boxes", {html: {include: "item29.html"}}],
    ["real", "Boxes", {html: {include: "item30.html"}}],
    ["real", "Boxes", {html: {include: "item31.html"}}],
    ["real", "Boxes", {html: {include: "item32.html"}}],
    ["real", "Boxes", {html: {include: "item33.html"}}],
    ["real", "Boxes", {html: {include: "item34.html"}}],
    ["real", "Boxes", {html: {include: "item35.html"}}],
    ["real", "Boxes", {html: {include: "item36.html"}}],
    ["real", "Boxes", {html: {include: "item37.html"}}],
    ["real", "Boxes", {html: {include: "item38.html"}}],
    ["real", "Boxes", {html: {include: "item39.html"}}],
    ["real", "Boxes", {html: {include: "item40.html"}}],
    ["real", "Boxes", {html: {include: "item41.html"}}],
    ["real", "Boxes", {html: {include: "item42.html"}}],
    ["real", "Boxes", {html: {include: "item43.html"}}],
    ["real", "Boxes", {html: {include: "item44.html"}}],
    ["real", "Boxes", {html: {include: "item45.html"}}],
    ["real", "Boxes", {html: {include: "item46.html"}}],
    ["real", "Boxes", {html: {include: "item47.html"}}],
    ["real", "Boxes", {html: {include: "item48.html"}}],
    ["real", "Boxes", {html: {include: "item49.html"}}],
    ["real", "Boxes", {html: {include: "item50.html"}}],
    ["real", "Boxes", {html: {include: "item51.html"}}],
    ["real", "Boxes", {html: {include: "item52.html"}}],
    ["real", "Boxes", {html: {include: "item53.html"}}],
    ["real", "Boxes", {html: {include: "item54.html"}}],
    ["real", "Boxes", {html: {include: "item55.html"}}],
    ["real", "Boxes", {html: {include: "item56.html"}}],
    ["real", "Boxes", {html: {include: "item57.html"}}],
    ["real", "Boxes", {html: {include: "item58.html"}}],
    ["real", "Boxes", {html: {include: "item59.html"}}],
    ["real", "Boxes", {html: {include: "item60.html"}}],
    ["real", "Boxes", {html: {include: "item61.html"}}],
    ["real", "Boxes", {html: {include: "item62.html"}}],
    ["real", "Boxes", {html: {include: "item63.html"}}],
    ["real", "Boxes", {html: {include: "item64.html"}}],
    ["real", "Boxes", {html: {include: "item65.html"}}],
    ["real", "Boxes", {html: {include: "item66.html"}}],
    ["real", "Boxes", {html: {include: "item67.html"}}],
    ["real", "Boxes", {html: {include: "item68.html"}}],
    ["real", "Boxes", {html: {include: "item69.html"}}],
    ["real", "Boxes", {html: {include: "item70.html"}}],
    ["real", "Boxes", {html: {include: "item71.html"}}],
    ["real", "Boxes", {html: {include: "item72.html"}}],
    ["real", "Boxes", {html: {include: "item73.html"}}],
    ["real", "Boxes", {html: {include: "item74.html"}}],
    ["real", "Boxes", {html: {include: "item75.html"}}],
    ["real", "Boxes", {html: {include: "item76.html"}}],
    ["real", "Boxes", {html: {include: "item77.html"}}],
    ["real", "Boxes", {html: {include: "item78.html"}}],
    ["real", "Boxes", {html: {include: "item79.html"}}],
    ["real", "Boxes", {html: {include: "item80.html"}}],
    ["real", "Boxes", {html: {include: "item81.html"}}],
    ["real", "Boxes", {html: {include: "item82.html"}}],
    ["real", "Boxes", {html: {include: "item83.html"}}],
    ["real", "Boxes", {html: {include: "item84.html"}}],
    ["real", "Boxes", {html: {include: "item85.html"}}],
    ["real", "Boxes", {html: {include: "item86.html"}}],
    ["real", "Boxes", {html: {include: "item87.html"}}],
    ["real", "Boxes", {html: {include: "item88.html"}}],
    ["real", "Boxes", {html: {include: "item89.html"}}],
    ["real", "Boxes", {html: {include: "item90.html"}}],
    ["real", "Boxes", {html: {include: "item91.html"}}],
    ["real", "Boxes", {html: {include: "item92.html"}}],
    ["real", "Boxes", {html: {include: "item93.html"}}],
    ["real", "Boxes", {html: {include: "item94.html"}}],
    ["real", "Boxes", {html: {include: "item95.html"}}],
    ["real", "Boxes", {html: {include: "item96.html"}}],
    ["real", "Boxes", {html: {include: "item97.html"}}],
    ["real", "Boxes", {html: {include: "item98.html"}}],
    ["real", "Boxes", {html: {include: "item99.html"}}],
    ["real", "Boxes", {html: {include: "item100.html"}}],
    ["real", "Boxes", {html: {include: "item101.html"}}],
    ["real", "Boxes", {html: {include: "item102.html"}}],
    ["real", "Boxes", {html: {include: "item103.html"}}],
    ["real", "Boxes", {html: {include: "item104.html"}}],
    ["real", "Boxes", {html: {include: "item105.html"}}],
    ["real", "Boxes", {html: {include: "item106.html"}}],
    ["real", "Boxes", {html: {include: "item107.html"}}],
    ["real", "Boxes", {html: {include: "item108.html"}}],
    ["real", "Boxes", {html: {include: "item109.html"}}],
    ["real", "Boxes", {html: {include: "item110.html"}}],
    ["real", "Boxes", {html: {include: "item111.html"}}],
    ["real", "Boxes", {html: {include: "item112.html"}}],
    ["real", "Boxes", {html: {include: "item113.html"}}],
    ["real", "Boxes", {html: {include: "item114.html"}}],
    ["real", "Boxes", {html: {include: "item115.html"}}],
    ["real", "Boxes", {html: {include: "item116.html"}}],
    ["real", "Boxes", {html: {include: "item117.html"}}],
    ["real", "Boxes", {html: {include: "item118.html"}}],
    ["real", "Boxes", {html: {include: "item119.html"}}],
    ["real", "Boxes", {html: {include: "item120.html"}}],
    ["real", "Boxes", {html: {include: "item121.html"}}],
    ["real", "Boxes", {html: {include: "item122.html"}}],
    ["real", "Boxes", {html: {include: "item123.html"}}],
    ["real", "Boxes", {html: {include: "item124.html"}}],
    ["real", "Boxes", {html: {include: "item125.html"}}],
    ["real", "Boxes", {html: {include: "item126.html"}}],

];
