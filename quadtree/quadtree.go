package quadtree

import (
    "fmt"
    "strings"
    "sync"
    "container/list"
)

// Vertex represents a point
type Vertex struct {
    X, Y int
}

// Rectangle is as you'd expect. Represented with left top and right bottom.
type Rectangle struct {
    LT, RB Vertex
}

type Object struct {
    Pos, Size Vertex
}

type QuadTreeInfo struct {
    MaxDepth int
}

type QuadTree struct {
    s *list.List
    r Rectangle
    depth int

    i *QuadTreeInfo

    // TODO: TreeStats(maxdepth, itemcount) (only one per quad tree)
    // NodeStats (items in children, etc)
    // capacity = inf

    NW, NE, SW, SE *QuadTree

    sync.Mutex
}

/* Tree creation functions */

func (q *QuadTree) Init(r Rectangle, depth int, i *QuadTreeInfo) *QuadTree {
    q.s = new(list.List)
    q.s.Init()

    q.r = r
    q.i = i
    q.depth = depth + 1

    if i.MaxDepth > q.depth {
        q.SubDivide()
    }

    return q
}

func (q *QuadTree) SubDivide() bool {
    if q.depth > q.i.MaxDepth {
        return false
    }
    w2 := (q.r.RB.X - q.r.LT.X) / 2
    h2 := (q.r.RB.Y - q.r.LT.Y) / 2

    NWArea := Rectangle{q.r.LT, Vertex{q.r.RB.X - w2, q.r.RB.Y - h2}}
    NEArea := Rectangle{Vertex{q.r.LT.X + w2, q.r.LT.Y},
        Vertex{q.r.RB.X, q.r.RB.Y - h2}}
    SWArea := Rectangle{Vertex{q.r.LT.X, q.r.LT.Y + h2},
        Vertex{q.r.RB.X - w2, q.r.RB.Y}}
    SEArea := Rectangle{Vertex{q.r.LT.X + w2, q.r.LT.Y + h2}, q.r.RB}

    q.NW = new(QuadTree).Init(NWArea, q.depth, q.i)
    q.NE = new(QuadTree).Init(NEArea, q.depth, q.i)
    q.SW = new(QuadTree).Init(SWArea, q.depth, q.i)
    q.SE = new(QuadTree).Init(SEArea, q.depth, q.i)

    return true
}


/* Add*() Functions */

// Returns true if the node can contain the object by looking at size and pos.
func (q * QuadTree) CanContain(o *Object) bool {
    s := o.Size
    p := o.Pos
    return p.X >= q.r.LT.X && p.Y >= q.r.LT.Y &&
            s.X + p.X <= q.r.RB.X && s.Y + p.Y <= q.r.RB.Y
}

func (q* QuadTree) AddObjects(objchan chan *Object) {
    var (
        objcount = 0
        addcount = 0
        addchan = make(chan bool)
        done = false
    )

    out:
    for {
        select {
        case obj, ok := <-objchan:
            if !ok {
                done = true
            } else {
                fmt.Println("obj")
                objcount += 1
                go q.Add(obj, addchan)
            }
        case add := <-addchan:
            fmt.Println("add", add, addcount, objcount)
            if add {
                addcount += 1
            } else {
                fmt.Println("WHOOPS")
                panic("Add was false")
            }
            if done && addcount == objcount {
                fmt.Println("Breaking, done=", done)
                break out
            }
        }
    }

    fmt.Println("Objcount:", objcount, "Addcount:", addcount)
}

func (q *QuadTree) Add(o *Object, addchan chan bool) {
    added := true
    if q.NW != nil {
        switch {
        case q.NW.CanContain(o): q.NW.Add(o, addchan)
        case q.NE.CanContain(o): q.NE.Add(o, addchan)
        case q.SW.CanContain(o): q.SW.Add(o, addchan)
        case q.SE.CanContain(o): q.SE.Add(o, addchan)
        default:
            // Doesn't fit in children. Add here.
            added = false
        }
    } else {
        // No children. Add here.
        added = false
    }

    if !added {
        q.Lock()

        q.s.PushBack(o)
        added = true

        q.Unlock()
    }

    addchan <- added
}


func (q *QuadTree) Print(d int) {
    prefix := strings.Repeat("-", d*2)

    fmt.Println(prefix, q.r, "#", q.s.Len())
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        fmt.Println(prefix, " *", o)
    }

    if q.NW != nil {
        q.NW.Print(d+1)
        q.NE.Print(d+1)
        q.SW.Print(d+1)
        q.SE.Print(d+1)
    }
}
