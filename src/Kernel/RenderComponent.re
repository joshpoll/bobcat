/* let computeSVGTransform =
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

   let svgTransform = (uid, transform, bbox, r) => {
     let transform = computeSVGTransform(transform, bbox);
     <g id={uid ++ "__node"} transform> r </g>;
   };

   type mouseMove = {
     clientX: float,
     clientY: float,
   };

   [@react.component]
   let rec make = (~node: RenderNodes.node) => {
     // EventListener.useEventListener("mousemove", ({clientX, clientY}) => {
     //   Js.log3("mousemove", clientX, clientY)
     // });
     // Js.log(EventListener.useEventListener("mousemove", _ => ()));
     /* None, */

     let RenderNodes.{uid, nodes, links, globalTransform, bbox, render} = node;
     <g id=uid>
       {render(bbox, links) |> svgTransform(uid, globalTransform, bbox)}
       <g id={uid ++ "__children"}>
         {List.map(node => make({"node": node}), nodes) |> Array.of_list |> React.array}
       </g>
     </g>;
   }; */