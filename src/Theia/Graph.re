module MS = Belt.Map.String;

let graphLayout =
    (
      ~constraints,
      ~gap,
      ~linkDistance,
      bboxes: list((UID.t, Node.bbox)),
      links: list(Link.layout),
    ) => {
  let uids = List.map(((uid, _)) => uid, bboxes);
  let nodes =
    List.map(
      ((uid, sizeOffset)) =>
        (
          uid,
          SetCoLa.{
            width: sizeOffset->Rectangle.width,
            height: sizeOffset->Rectangle.height,
            custom: Js.Obj.empty(),
          },
        ),
      bboxes,
    );
  let links =
    List.map(
      (Link.{source, target}: Link.layout) =>
        WebCoLa.{
          source: NN(Fn.find(source, uids)),
          target: NN(Fn.find(target, uids)),
          length: None,
        },
      links,
    )
    |> Array.of_list;

  let setCoLaGraph =
    SetCoLa.setCola
    ->SetCoLa.nodes(List.map(((_, bbox)) => bbox, nodes) |> Array.of_list)
    ->SetCoLa.links(links)
    ->SetCoLa.constraints(constraints);

  let setCoLaGraph =
    switch (gap) {
    | None => setCoLaGraph
    | Some(gap) => setCoLaGraph->SetCoLa.gap(gap)
    };

  let setCoLaGraph = setCoLaGraph->SetCoLa.layout;

  let colaGraph =
    WebCoLa.colaLayout()
    ->WebCoLa.nodes(setCoLaGraph.nodes)
    ->WebCoLa.links(setCoLaGraph.links)
    ->WebCoLa.constraints(setCoLaGraph.constraints)
    ->WebCoLa.avoidOverlaps(true);

  let colaGraph =
    switch (linkDistance) {
    | None => colaGraph
    | Some(linkDistance) => colaGraph->WebCoLa.linkDistance(linkDistance)
    };

  let colaGraph = colaGraph->WebCoLa.start(Some(50.), Some(100.), Some(200.), None);

  let nodes =
    colaGraph->WebCoLa.getNodes |> Array.to_list |> List.filter((WebCoLa.{temp}) => !temp);
  List.map(
    ((uid, bbox)) =>
      (
        uid,
        {
          let colaNode = List.nth(nodes, Fn.find(uid, uids));
          Transform.{
            scale: {
              x: 1.,
              y: 1.,
            },
            translate: {
              x: colaNode.x -. bbox->Rectangle.x1,
              y: colaNode.y -. bbox->Rectangle.y1,
            },
          };
        },
      ),
    bboxes,
  )
  |> Array.of_list
  |> MS.fromArray;
};

let mk = (~uid=?, ~tag=?, ~nodes, ~links, ~gap=?, ~linkDistance=?, ~constraints) =>
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes,
    ~links,
    ~layout=graphLayout(~constraints, ~gap, ~linkDistance),
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->Rectangle.union_list,
    ~nodeRender=_ => React.null,
  );