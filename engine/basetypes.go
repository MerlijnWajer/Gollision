package engine

// Vertex represents a point
type Vertex struct {
	X, Y int
}

// Rectangle is as you'd expect. Represented with left top and right bottom.
type Rectangle struct {
	LT, RB Vertex
}

