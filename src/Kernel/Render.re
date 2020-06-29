/* TODO: factor out this computation so it can be shared by Render and TransitionNode */

let computeSVGTransform =
    ({translate: {x: tx, y: ty}, scale: {x: sx, y: sy}}: Node.transform, bbox) => {
  /* https://css-tricks.com/transforms-on-svg-elements/ */
  let scale =
    "translate("
    ++ Js.Float.toString(bbox->Rectangle.x1 +. bbox->Rectangle.width /. 2.)
    ++ ", "
    ++ Js.Float.toString(bbox->Rectangle.y1 +. bbox->Rectangle.height /. 2.)
    ++ ")";
  let scale = scale ++ " " ++ {j|scale($sx, $sy)|j};
  let scale =
    scale
    ++ " "
    ++ "translate("
    ++ Js.Float.toString(-. (bbox->Rectangle.x1 +. bbox->Rectangle.width /. 2.))
    ++ ", "
    ++ Js.Float.toString(-. (bbox->Rectangle.y1 +. bbox->Rectangle.height /. 2.))
    ++ ")";

  let translate = {j|translate($tx, $ty)|j};

  scale ++ " " ++ translate;
};

/* Doesn't allow for late/dynamic visual changes. */
let rec convert = (RenderLinks.{uid, nodes, links, globalTransform, bbox, nodeRender}) => {
  let transform = computeSVGTransform(globalTransform, bbox);
  <g id=uid key=uid>
    <g id={uid ++ "__node"} transform> {nodeRender(bbox)} </g>
    <g id={uid ++ "__children"}> {List.map(convert, nodes) |> Array.of_list |> React.array} </g>
    <g id={uid ++ "__links"}>
      {links
       |> List.mapi((i, l) => <g key={uid ++ string_of_int(i)}> l </g>)
       |> Array.of_list
       |> React.array}
    </g>
  </g>;
};
