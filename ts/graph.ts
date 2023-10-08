// nodes => colorIx => filterByColorWeight
// k = minimal weight
// loop
//     - nodes => edges => nodeWeights
//     - drop nodes with weights < k

// colorNeighbourhood
// nodeNeighbourhood

type Color = number & {readonly _tag: unique symbol}

type NodeRef = number & {readonly _tag: unique symbol}
type NodeLabel = Color[]
type Nodes = Map<NodeRef, NodeLabel>

type EdgeRef = number & {readonly _tag: unique symbol}
type EdgeLabel = {a: NodeRef, b: NodeRef, c: Color}
type Edges = Map<EdgeRef, EdgeLabel>

type ColorToNodes = Map<Color, NodeRef[]>

const N = 7825;

function generateNodes(n: number): Nodes {
    const ns: Nodes = new Map();
    let nextRef = 0;
    for(let a = 1; a < n; a++) {
        for(let b = a+1; b < n; b++) {
            const ab = a*a + b*b;
            const c = Math.floor(Math.sqrt(a*a + b*b));
            if(c <= n && ab == c*c) {
                ns.set(nextRef++ as NodeRef, [a,b,c] as Color[]);
            }
        }
    }
    return ns;
}

function groupNodesByColors(ns: Nodes): ColorToNodes {
    const ix: ColorToNodes = new Map();
    for(let ref of ns.keys()) {
        for(let c of ns.get(ref)!) {
            const ixc = ix.get(c);
            ixc ? ixc.push(ref) : ix.set(c, [ref]);
        }
    }
    return ix;
}

function filterByColorWeight(nodes: Nodes, w: number): Nodes {
    const ns: Nodes = new Map(nodes.entries());
    while(true) {
        let done = true;
        const cs = groupNodesByColors(ns);
        for(let refs of cs.values()) {
            if(refs.length < w) {
                refs.forEach(r => ns.delete(r))
                done = false;
            }
        }
        if(done) break;
    }
    return ns;
}

function makeEdgesFromColors(cs: ColorToNodes): Edges {
    const es: Edges = new Map();
    let nextRef = 0;
    for(let c of cs.keys()) {
        let nodes = cs.get(c)!;
        for(let i = 0; i < nodes.length; i++) {
            for(let j = i+1; j < nodes.length; j++) {
                es.set(nextRef++ as EdgeRef, {a: nodes[i], b: nodes[j], c});
            }
        }
    }
    return es;
}


type NodeWeights = Map<NodeRef, number>

function nodeWeights(es: Edges): NodeWeights {
    const ws: NodeWeights = new Map();
    for(let e of es.values()) {
        const a = ws.get(e.a);
        ws.set(e.a, a ? a+1 : 1);
        const b = ws.get(e.b);
        ws.set(e.b, b ? b+1 : 1);
    }
    return ws;
}


function kCore(ns: Nodes, k: number) {
    const core: Nodes = new Map(ns.entries());
    const shell: NodeRef[] = [];
    while(true) {
        const cIx = groupNodesByColors(core);
        const es = makeEdgesFromColors(cIx);
        let done = true;
        nodeWeights(es).forEach((w, ref) => {
            if (w < k) {
                shell.push(ref);
                core.delete(ref);
                done = false;
            }
        });
        if(done) break;
    }
    return {shell, core};
}

function colorNeighbourhood(
    rootColor: Color,
    radius: number,
    cs: ColorToNodes,
    ns: Nodes
): Set<NodeRef> {
    const res = new Set<NodeRef>();
    let nextColors = new Set<Color>([rootColor]);
    while(radius-- > 0) {
        let nextNodes = new Set<NodeRef>();
        for(let c of nextColors) {
            for(let n of cs.get(c)!) {
                nextNodes.add(n);
                res.add(n);
            }
        }
        if(radius == 0) break;
        nextColors = new Set<Color>(
            [...nextNodes].map(n => ns.get(n)!).flat()
        );
    }
    return res;
}


let ns = generateNodes(N);
console.log("number of triples", ns.size);
ns = filterByColorWeight(ns, 2);
console.log("number of triples without pendants", ns.size);
const colorIx = groupNodesByColors(ns);

let nh = colorNeighbourhood(15 as Color, 3, colorIx, ns);
console.log("2-neighbourhood of 15", [...nh].map(r => ns.get(r)));

let k = 1;
while(true) {
    let res = kCore(ns, k);
    if(res.shell.length != 0) {
        console.log(`${k}:`, res.shell.length, res.core.size);
    }
    if(res.core.size == 0) break;
    ns = res.core;
    k++;
}

console.log("tightest core:", [...ns.values()]);
console.log("number of variables:", (new Set([...ns.values()].flat())).size);