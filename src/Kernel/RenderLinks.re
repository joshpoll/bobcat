/* TODO: probably needs to come after rendering nodes b/c now using a global frame so we don't know where they are? */
type node('a) = {
  uid: UID.t,
  tag: option('a),
  nodes: list(node('a)),
  links: list(React.element),
  globalTransform: Node.transform,
  bbox: Node.bbox,
  nodeRender: Node.bbox => React.element,
};

let rec computeTransform = (node: GlobalTransform.node('a), path) =>
  switch (path) {
  | [] => Rectangle.transform(node.bbox, node.globalTransform)
  | [h, ...path] =>
    let node = List.find((GlobalTransform.{uid}) => h == uid, node.nodes);
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
    lr(~source, ~target);
  };

let rec convert =
        (GlobalTransform.{uid, tag, nodes, links, globalTransform, bbox, nodeRender} as n)
        : node('a) => {
  let nodes = List.map(convert, nodes);
  let links = List.map(renderLink(n), links);
  {uid, tag, nodes, links, globalTransform, bbox, nodeRender};
};
