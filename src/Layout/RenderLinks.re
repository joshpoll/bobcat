/* TODO: probably needs to come after rendering nodes b/c now using a global frame so we don't know where they are? */
type node('a) = {
  uid: UID.t,
  tag: option('a),
  nodes: list(node('a)),
  links: list(React.element),
  transform: Node.transform,
  bbox: Node.bbox,
  nodeRender: Node.bbox => React.element,
};

let rec computeTransform = (node: LayoutIR.node('a), path) =>
  switch (path) {
  | [] => Rectangle.transform(node.bbox, node.transform)
  | [h, ...path] =>
    let node = switch (List.find((LayoutIR.{uid}) => h == uid, node.nodes)) {
      | node => node
      | exception Not_found => 
      Js.log2("couldn't find `" ++ h ++ "` in", node.nodes);
      failwith("computeTransform coudn't find the node")
    };
    computeTransform(node, path);
  };

let renderLink = (node, Link.{source, target, linkRender}: Link.lcaPath): React.element =>
  switch (linkRender) {
  | None => <> </>
  | Some(lr) =>
    let key = List.fold_left((++), "", source) ++ List.fold_left((++), "", target);
    let source = computeTransform(node, source);
    let target = computeTransform(node, target);
    /* <g key> {lr(~source, ~target)} </g>; */
    Js.log2("rendered link", lr(~source, ~target));
    lr(~source, ~target);
  };

let rec lower =
        (LayoutIR.{uid, tag, nodes, links, transform, bbox, nodeRender} as n)
        : node('a) => {
  let nodes = List.map(lower, nodes);
  let links = List.map(renderLink(n), links);
  {uid, tag, nodes, links, transform, bbox, nodeRender};
};
