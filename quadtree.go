package main

import (
    "fmt"
    "container/list"
)

type QuadTree struct {
    s *list.List
    r Rectangle
    depth int

    NW, NE, SW, SE *QuadTree
}

// Initialise node
func (q *QuadTree) Init(r Rectangle, depth int) *QuadTree {
    q.s = new(list.List)
    q.s.Init()

    q.r = r
    q.depth = depth + 1

    /* NW, NE, SW, SE are nil initially */
    return q
}

// Returns true if the node can contain the object by looking at size and pos.
func (q * QuadTree) CanContain(o *Object) bool {
    s := o.Size()
    p := o.Pos()
    return p.x >= q.r.lt.x && p.y >= q.r.lt.y &&
            s.x + p.x <= q.r.rb.x && s.y + p.y <= q.r.rb.y
}

// Create four children
func (q *QuadTree) SubDivide() bool {
    if q.depth > maxdepth {
        return false
    }
    w2 := (q.r.rb.x - q.r.lt.x) / 2
    h2 := (q.r.rb.y - q.r.lt.y) / 2

    NWArea := Rectangle{q.r.lt, Vertex{q.r.rb.x - w2, q.r.rb.y - h2}}
    NEArea := Rectangle{Vertex{q.r.lt.x + w2, q.r.lt.y},
        Vertex{q.r.rb.x, q.r.rb.y - h2}}
    SWArea := Rectangle{Vertex{q.r.lt.x, q.r.lt.y + h2},
        Vertex{q.r.rb.x - w2, q.r.rb.y}}
    SEArea := Rectangle{Vertex{q.r.lt.x + w2, q.r.lt.y + h2}, q.r.rb}

    q.NW = new(QuadTree).Init(NWArea, q.depth)
    q.NE = new(QuadTree).Init(NEArea, q.depth)
    q.SW = new(QuadTree).Init(SWArea, q.depth)
    q.SE = new(QuadTree).Init(SEArea, q.depth)

    return true
}

// Add to double linked list in node. Returns false is the list if full.
func (q *QuadTree) AddSimple(o * Object) bool {
    l := q.s
    if l.Len() >= capacity {
        return false
    }
    l.PushBack(o)
    return true
}

// Reorder children. Try to push children down the tree to make space in
// the list.
func (q *QuadTree) Clean() bool {
    l := new(list.List)
    l.Init()
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        if !q.AddToChild(o) {
            l.PushBack(o)
        }
    }

    q.s = l

    return true
}

// Add to children if possible. Returns true on success
func (q *QuadTree) AddToChild(o *Object) bool {
    switch {
        case q.NW.CanContain(o): return q.NW.Add(o)
        case q.NE.CanContain(o): return q.NE.Add(o)
        case q.SE.CanContain(o): return q.SE.Add(o)
        case q.SW.CanContain(o): return q.SW.Add(o)
    }
    return false
}

// Add object to Tree
func (q *QuadTree) Add(o *Object) bool {
    if !q.CanContain(o) {
        return false
    }
    if q.AddSimple(o) {
        return true
    }

    /* List is full, divide */
    if q.NW == nil {
        if !q.SubDivide() {
            fmt.Println("SubDivide failed")
            return false
        }

        q.Clean()
    }

    success := q.AddToChild(o)
    /* If this failed, we could try a q.Clean() */

    if !success {
        return false
        //panic("Failed to add")
    }

    return success
}
