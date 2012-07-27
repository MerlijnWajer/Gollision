package engine

import (
	"sync"
)

func Collides(o, o2 *Object) bool {
	switch {
	case o.Pos.Y+o.Size.Y < o2.Pos.Y:
		return false
	case o.Pos.Y > o2.Pos.Y+o2.Size.Y:
		return false
	case o.Pos.X+o.Size.X < o2.Pos.X:
		return false
	case o.Pos.X > o2.Pos.X+o2.Size.X:
		return false
	}

	return true
}

// TODO: Make this concurrent
func FindCollision(q *QuadTree, obj *Object, out chan *Object) {
	for e := q.s.Front(); e != nil; e = e.Next() {
		o := e.Value.(*Object)

		// We always collide with ourself
		if o == obj {
			continue
		}

        // TODO: Inline this? Would probably save quite some time...
        // This is the only place we use it as well.
		if Collides(o, obj) {
			out <- o
			out <- obj
		}
	}

	if q.NW != nil {
		FindCollision(q.NW, obj, out)
		FindCollision(q.NE, obj, out)
		FindCollision(q.SW, obj, out)
		FindCollision(q.SE, obj, out)
	}
}

// XXX: This isn't actually useful for our collision detection.
// Just use this to test the collision functions.
func Collisions(q *QuadTree, out chan *Object) {
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

	if q.NW != nil {
        Collisions(q.NW, out)
        Collisions(q.NE, out)
        Collisions(q.SW, out)
        Collisions(q.SE, out)
	}
}

