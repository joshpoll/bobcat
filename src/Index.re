// Entry point

[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let style = document##createElement("style");
document##head##appendChild(style);
style##innerHTML #= ExampleStyles.style;

let makeContainer = text => {
  let container = document##createElement("div");
  container##className #= "container";

  let title = document##createElement("div");
  title##className #= "containerTitle";
  title##innerText #= text;

  let content = document##createElement("div");
  content##className #= "containerContent";

  let () = container##appendChild(title);
  let () = container##appendChild(content);
  let () = document##body##appendChild(container);

  content;
};

let x = 10.;
let y = 30.;
let width = 200.;
let height = 100.;
ReactDOMRe.render(
  <svg>
    {Kernel.render(
       Theia.atom(
         ~tag=None,
         <rect
           x={Js.Float.toString(x)}
           y={Js.Float.toString(y)}
           width={Js.Float.toString(width)}
           height={Js.Float.toString(height)}
           stroke="black"
           strokeDasharray="4"
           strokeWidth="3"
           fill="magenta"
         />,
         Rectangle.fromPointSize(~x, ~y, ~width, ~height),
       ),
     )}
  </svg>,
  makeContainer("Theia Atom"),
);

ReactDOMRe.render(
  <svg>
    {Kernel.render(
       Theia.box(
         ~dx=5.,
         ~dy=5.,
         Theia.atom(
           <rect
             x={Js.Float.toString(x)}
             y={Js.Float.toString(y)}
             width={Js.Float.toString(width)}
             height={Js.Float.toString(height)}
             stroke="black"
             strokeDasharray="4"
             strokeWidth="3"
             fill="magenta"
           />,
           Rectangle.fromPointSize(~x, ~y, ~width, ~height),
         ),
         [],
       ),
     )}
  </svg>,
  makeContainer("Theia Box"),
);

ReactDOMRe.render(<svg> {Kernel.render(Theia.str("foo"))} </svg>, makeContainer("Theia Str"));

ReactDOMRe.render(
  <svg>
    <g transform="translate(50, 50)">
      {Kernel.render(
         Theia.hSeq([Theia.vSeq([Theia.str("foo"), Theia.str("foo")]), Theia.str("bar")]),
       )}
    </g>
  </svg>,
  makeContainer("Nested Sequences"),
);
