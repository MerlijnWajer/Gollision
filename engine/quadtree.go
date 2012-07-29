package engine

import (
	"container/list"
	"fmt"
	"strings"
	"sync"
)

type QuadTreeInfo struct {
	MaxDepth int
}

type WalkFunc func(e *list.Element)

type QuadTree struct {
	s     *list.List
	r     Rectangle
	depth int

	i *QuadTreeInfo

    parent *QuadTree

    // TODO: Parent?

	// TODO: TreeStats(maxdepth, itemcount) (only one per quad tree)
	// NodeStats (items in children, etc)
	// capacity = inf

	NW, NE, SW, SE *QuadTree

	sync.Mutex
}

/* Tree creation functions */

func (q *QuadTree) Init(r Rectangle, depth int, i *QuadTreeInfo, p *QuadTree) *QuadTree {
	q.s = new(list.List)
	q.s.Init()

	q.r = r
	q.i = i
	q.depth = depth + 1
    q.parent = p

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

	q.NW = new(QuadTree).Init(NWArea, q.depth, q.i, q)
	q.NE = new(QuadTree).Init(NEArea, q.depth, q.i, q)
	q.SW = new(QuadTree).Init(SWArea, q.depth, q.i, q)
	q.SE = new(QuadTree).Init(SEArea, q.depth, q.i, q)

	return true
}

/* Add*() Functions */

// Returns true if the node can contain the object by looking at size and pos.
func (q *QuadTree) CanContain(o *Object) bool {
	s := o.Size
	p := o.Pos
	return p.X >= q.r.LT.X && p.Y >= q.r.LT.Y &&
		s.X+p.X <= q.r.RB.X && s.Y+p.Y <= q.r.RB.Y
}

func (q *QuadTree) AddObjects(objchan chan *Object) {
	wg := new(sync.WaitGroup)

	for obj := range objchan {
		wg.Add(1)
		go q.Add(obj, wg)
	}
	wg.Wait()
}

func (q *QuadTree) Add(o *Object, wg *sync.WaitGroup) {
	added := true
	if q.NW != nil {
		switch {
		case q.NW.CanContain(o):
			q.NW.Add(o, wg)
			return
		case q.NE.CanContain(o):
			q.NE.Add(o, wg)
			return
		case q.SW.CanContain(o):
			q.SW.Add(o, wg)
			return
		case q.SE.CanContain(o):
			q.SE.Add(o, wg)
			return
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

		q.s.PushFront(o)
        o.CurrentNode = q
		added = true

		q.Unlock()
	}

	if wg != nil {
        wg.Done()
    }
}

// Move object to new pos
func (q *QuadTree) Move(e *list.Element) {
    o := e.Value.(*Object)
    // Always remove. In the worst case we're part of the root node.
    // Which always contains us. If we fit in the node, recursively add it.
    // If we don't fit, recursively move to the top until we fit.
    if !o.CurrentNode.CanContain(o) {
        o.CurrentNode.s.Remove(e)
        o.CurrentNode.performMove(o)
    }
}

func (q *QuadTree) performMove(o* Object) {
    if !q.CanContain(o) {
        if q.parent != nil {
            q.parent.performMove(o)
        } else {
            fmt.Println(o)
            panic("Item does not fit into any node!")
        }

    } else {
        q.Add(o, nil)
    }

}

// Use this to iterate over the tree.
func (q* QuadTree) Walk(w WalkFunc) {
    // XXX: This function currenly prefetches the next node, to
    // prevent failure in walking due to the current node
    // being deleted by the walk function.
    // This is a temporary fix, walking actually should never
    // be used on a chaning structure. A better solution could
    // possibly be to somehow mark nodes in need of movement.
    // Or introduce a special modification walker function.
    var n       *list.Element

	for e := q.s.Front(); e != nil; e = n {
        n = e.Next()
		//o := e.Value.(*Object)
        w(e)
	}

	if q.NW != nil {
		q.NW.Walk(w)
		q.NE.Walk(w)
		q.SW.Walk(w)
		q.SE.Walk(w)
	}
}

func (q *QuadTree) Print(d int) {
	prefix := strings.Repeat("-", d*2)

	fmt.Println(prefix, q.r, "#", q.s.Len())
	for e := q.s.Front(); e != nil; e = e.Next() {
		o := e.Value.(*Object)
		fmt.Println(prefix, " *", o)
	}

	if q.NW != nil {
		q.NW.Print(d + 1)
		q.NE.Print(d + 1)
		q.SW.Print(d + 1)
		q.SE.Print(d + 1)
	}
}
