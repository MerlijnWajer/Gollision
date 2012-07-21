package engine

import (
    "sync"
)

func Collisions(q *QuadTree, out chan *Object, gogo int) {
    findcol_wg := new(sync.WaitGroup)
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)

        FindCollision(q, o, out)
        findcol_wg.Add(1)
        go func() {
            FindCollision(q, o, out)
            findcol_wg.Done()
        }()
    }

    findcol_wg.Wait()

    if (q.NW != nil) {
        if (gogo > 0) {
            wg := new(sync.WaitGroup)
            wg.Add(1)
            go func() {
                Collisions(q.NW, out, gogo-1)
                wg.Done()
            }()
            wg.Add(1)
            go func() {
                Collisions(q.NE, out, gogo-1)
                wg.Done()
            }()
            wg.Add(1)
            go func() {
                Collisions(q.SW, out, gogo-1)
                wg.Done()
            }()
            wg.Add(1)
            go func() {
                Collisions(q.SE, out, gogo-1)
                wg.Done()
            }()

            wg.Wait()
        } else {
            Collisions(q.NW, out, gogo-1)
            Collisions(q.NE, out, gogo-1)
            Collisions(q.SW, out, gogo-1)
            Collisions(q.SE, out, gogo-1)
        }
    }
}

func Collides(o, o2 *Object) bool {
    switch {
        case o.Pos.Y + o.Size.Y < o2.Pos.Y:
            return false
        case o.Pos.Y > o2.Pos.Y + o2.Size.Y:
            return false
        case o.Pos.X + o.Size.X < o2.Pos.X:
            return false
        case o.Pos.X > o2.Pos.X + o2.Size.X:
            return false
    }

    return true
}

func FindCollision(q* QuadTree, obj *Object, out chan *Object) {
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)

        // We always collide with ourself
        if o == obj {
            continue
        }
        if Collides(o, obj) {
            out <- o
            out <- obj
        }
    }

    if (q.NW != nil) {
        FindCollision(q.NW, obj, out)
        FindCollision(q.NE, obj, out)
        FindCollision(q.SW, obj, out)
        FindCollision(q.SE, obj, out)
    }
}
