type node('a) = {
  uid: UID.t,
  /* TODO: erase this field here? intended to be used only for modifications to layout before rendering */
  tag: option('a),
  nodes: list(node('a)),
  links: list(Link.lcaPath),
  /* transform relative to global frame. useful for animation */
  globalTransform: Node.transform,
  bbox: Node.bbox,
  nodeRender: Node.bbox => React.element,
};

let rec computeGlobalTransformAux =
        (globalTransform, Layout.{uid, tag, nodes, links, transform, bbox, nodeRender}) => {
  let globalTransform = globalTransform->Transform.compose(transform);
  let nodes = List.map(computeGlobalTransformAux(globalTransform), nodes);
  {uid, tag, nodes, links, globalTransform, bbox, nodeRender};
};

let convert = n => computeGlobalTransformAux(Transform.init, n);

let rec findNodeByUID = (uid, {uid: candidate, nodes} as n) =>
  if (uid == candidate) {
    Some(n);
  } else {
    List.fold_left(
      (on, n) =>
        switch (on) {
        | None => findNodeByUID(uid, n)
        | Some(n) => Some(n)
        },
      None,
      nodes,
    );
  };

let findNodeByUIDExn = (uid, n) =>
  switch (findNodeByUID(uid, n)) {
  | None => failwith("couldn't find flow uid: " ++ uid)
  | Some(n) => n
  };

let rec findNodeByPathExn = (path, {nodes} as n) =>
  switch (path) {
  | [] => n
  | [i, ...path] => findNodeByPathExn(path, List.nth(nodes, i))
  };
