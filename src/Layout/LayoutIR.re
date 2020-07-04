type node('a) = {
  uid: UID.t,
  tag: option('a),
  nodes: list(node('a)),
  links: list(Link.lcaPath),
  transform: Node.transform,
  bbox: Node.bbox,
  nodeRender: Node.bbox => React.element,
};
