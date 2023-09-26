package astar;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.PriorityQueue;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Queue;


class AStar {
    private final List<Node> open;
    private final List<Node> closed;
    private final List<Node> path;
    private final int[][] maze;
    private Node now;
    private final int xstart;
    private final int ystart;
    private int xend, yend;
    private final boolean diag;

    // Node class for convienience
    static class Node implements Comparable {
        public Node parent;
        public int x, y;
        public double g;
        public double h;
        Node(Node parent, int xpos, int ypos, double g, double h) {
            this.parent = parent;
            this.x = xpos;
            this.y = ypos;
            this.g = g;
            this.h = h;
       }
       // Compare by f value (g + h)
       @Override
       public int compareTo(Object o) {
           Node that = (Node) o;
           return (int)((this.g + this.h) - (that.g + that.h));
       }
   }

    AStar(int[][] maze, int xstart, int ystart, boolean diag) {
        this.open = new ArrayList<>();
        this.closed = new ArrayList<>();
        this.path = new ArrayList<>();
        this.maze = maze;
        this.now = new Node(null, xstart, ystart, 0, 0);
        this.xstart = xstart;
        this.ystart = ystart;
        this.diag = diag;
    }
    /*
    ** Finds path to xend/yend or returns null
    **
    ** @param (int) xend coordinates of the target position
    ** @param (int) yend
    ** @return (List<Node> | null) the path
    */
    public List<Node> findPathTo(int xend, int yend) {
        this.xend = xend;
        this.yend = yend;
        this.closed.add(this.now);
        addNeigborsToOpenList();
        while (this.now.x != this.xend || this.now.y != this.yend) {
            if (this.open.isEmpty()) { // Nothing to examine
                return null;
            }
            this.now = this.open.get(0); // get first node (lowest f score)
            this.open.remove(0); // remove it
            this.closed.add(this.now); // and add to the closed
            addNeigborsToOpenList();
        }
        this.path.add(0, this.now);
        while (this.now.x != this.xstart || this.now.y != this.ystart) {
            this.now = this.now.parent;
            this.path.add(0, this.now);
        }
        return this.path;
    }
    /*
    **This function is the step of expanding nodes
    **
    **
    */ 
    public void expandAStar(int[][] maze, int xstart, int ystart, boolean diag){
        Queue<Mazecoord> exploreNodes = new LinkedList<Mazecoord>();
        if(maze[stateNode.getR()][stateNode.getC()] == 2){
            if(isNodeILegal(stateNode, stateNode.expandDirection())){     
                exploreNodes.add(stateNode.expandDirection());
         }
        }
     }

    /*
    ** Calculate distance for goal three methods shown.
    ** 
    ** 
    */
    public void AStarSearch()
	{
        this.start.setCostToGoal(this.start.calculateCost(this.goal));
        this.start.setPathCost(0);
        this.start.setAStartCost(this.start.getPathCost() + this.start.getCostToGoal());
        Mazecoord intialNode = this.start;
        Mazecoord stateNode = intialNode;
        frontier.add(intialNode);
        //explored<Queue> is empty
        while (true){
            if(frontier.isEmpty()){
                System.out.println("fail");
                System.out.println(explored.size());
                System.exit(-1);
            }
        }
     }
    /*
    ** Second method.
    ** 
    ** 
    */
    /**
     * calculate the cost from current node to goal.
     * @param goal : goal
     * @return : cost from current node to goal. use Manhattan distance.
     */
    public int calculateCost(Mazecoord goal)
    {
        int rState = this.getR();
        int rGoal = goal.getR();
        int diffR = rState - rGoal;
        int diffC = this.getC() - goal.getC();
        if(diffR * diffC > 0) {     // same sign
            return Math.abs(diffR) + Math.abs(diffC);
        } else {
            return Math.max(Math.abs(diffR), Math.abs(diffC));
        }
    }

    public Coord getFather(){
        return this.father;
    }

    public void setFather(Mazecoord node){
        this.father = node;
    }

   public int getAStartCost() {
        return AStartCost;
    }

    public void setAStartCost(int aStartCost) {
        AStartCost = aStartCost;
    }

    public int getCostToGoal() {
        return costToGoal;
    }

    public void setCostToGoal(int costToGoal) {
        this.costToGoal = costToGoal;
    }
    /*
    ** Third method.
    ** 
    ** 
    */
    private double distance(int dx, int dy) {
        if (this.diag) { // if diagonal movement is alloweed
            return Math.hypot(this.now.x + dx - this.xend, this.now.y + dy - this.yend); // return hypothenuse
        } else {
            return Math.abs(this.now.x + dx - this.xend) + Math.abs(this.now.y + dy - this.yend); // else return "Manhattan distance"
        }
    }
    private void addNeigborsToOpenList() {
        Node node;
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                if (!this.diag && x != 0 && y != 0) {
                    continue; // skip if diagonal movement is not allowed
                }
                node = new Node(this.now, this.now.x + x, this.now.y + y, this.now.g, this.distance(x, y));
                if ((x != 0 || y != 0) // not this.now
                    && this.now.x + x >= 0 && this.now.x + x < this.maze[0].length // check maze boundaries
                    && this.now.y + y >= 0 && this.now.y + y < this.maze.length
                    && this.maze[this.now.y + y][this.now.x + x] != -1 // check if square is walkable
                    && !findNeighborInList(this.open, node) && !findNeighborInList(this.closed, node)) { // if not already done
                        node.g = node.parent.g + 1.; // Horizontal/vertical cost = 1.0
                        node.g += maze[this.now.y + y][this.now.x + x]; // add movement cost for this square

                        // diagonal cost = sqrt(hor_cost² + vert_cost²)
                        // in this example the cost would be 12.2 instead of 11
                        /*
                        if (diag && x != 0 && y != 0) {
                            node.g += .4;	// Diagonal movement cost = 1.4
                        }
                        */
                        this.open.add(node);
                }
            }
        }
        Collections.sort(this.open);
    }

    public static void main(String[] args) {
        // -1 = blocked
        // 0+ = additional movement cost
        int[][] maze = {
            {  0,  0,  0,  0,  0,  0,  0,  0},
            {  0,  0,  0,  0,  0,  0,  0,  0},
            {  0,  0,  0,100,100,100,  0,  0},
            {  0,  0,  0,  0,  0,100,  0,  0},
            {  0,  0,100,  0,  0,100,  0,  0},
            {  0,  0,100,  0,  0,100,  0,  0},
            {  0,  0,100,100,100,100,  0,  0},
            {  0,  0,  0,  0,  0,  0,  0,  0},
        };
        AStar as = new AStar(maze, 0, 0, true);
        List<Node> path = as.findPathTo(7, 7);
        if (path != null) {
            path.forEach((n) -> {
                System.out.print("[" + n.x + ", " + n.y + "] ");
                maze[n.y][n.x] = -1;
            });
            System.out.printf("\nTotal cost: %.02f\n", path.get(path.size() - 1).g);

            for (int[] maze_row : maze) {
                for (int maze_entry : maze_row) {
                    switch (maze_entry) {
                        case 0:
                            System.out.print("_");
                            break;
                        case -1:
                            System.out.print("*");
                            break;
                        default:
                            System.out.print("#");
                    }
                }
                System.out.println();
            }
        }
    }
}
