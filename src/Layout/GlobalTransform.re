open LayoutIR;

let rec computeGlobalTransformAux =
        (globalTransform, {uid, tag, nodes, links, transform, bbox, nodeRender}) => {
  let transform = globalTransform->Transform.compose(transform);
  let nodes = List.map(computeGlobalTransformAux(transform), nodes);
  {uid, tag, nodes, links, transform, bbox, nodeRender};
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
