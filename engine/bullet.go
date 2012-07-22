package engine

// This is currently the structure used for all objects.
// I made the Object into an interface in another branch.
// The downside is that you can't directly access Pos and Size.
// Changing it to Pos() and Size() will make the code *TWICE* as slow.
// Just use Object for everything for now.
type Object struct {
	Pos, Size Vertex
	Collides  bool

    // backref to node in the tree?
}
