#ifndef PARETO_HH
#define PARETO_HH

#include <assert.h>
#include <stdio.h>
#include <limits>
#include <vector>
#include <utility>

/**
 * 2D Pareto set in a sorted vector.
 *
 */


template<typename W>

class pareto {
    // 2D Pareto set

public:

    struct point {
        W x;
        W y;
        point() : x(1000000), y(1000000) {}
        point(W xx, W yy) : x(xx), y(yy) {}
        point add(W dx, W dy) {
            return point(x + dx, y + dy);
        }
        point(const point &p) : x(p.x), y(p.y) {}
    };
    
    std::vector<point> pts;

    pareto(size_t cap=48) { pts.reserve(cap); }

    pareto(const pareto &p) : pts(p.pts) {}

    pareto& operator=(const pareto& p) {
        if (this != &p) {
            pts = p.pts;
        }
        return *this;
    }

    bool add(W x, W y) { // returns true if point was added
        bool insert = false;
        auto pts_end = pts.end();
        auto pos = pts_end;
        point tmp(x, y);
        size_t n_dom = 0;
        for (auto p = pts.begin(); p != pts_end; ++p) {
            if (p->x <= x && p->y <= y) return false; // dominated
            if (p->x < x) continue;
            // x <= p->x
            if (y <= p->y) { // *p is dom
                ++n_dom;
                if ( ! insert) {
                    std::swap(*p, tmp);
                    insert = true;
                } else if (pos == pts_end) {
                    pos = p;
                }
            } else {
                if ( ! insert) {
                    pts.insert(p, tmp);
                    return true;
                } else if (pos != pts_end) {
                    std::swap(*pos, *p);
                    ++pos;
                }
            }
        }
        if (insert) {
            pts.resize(pts.size() - n_dom + 1);
        } else {
            pts.push_back(tmp);
        }
        return true;
    }

    bool del_dominated(W x, W y) { // returns true if x,y dominates some elt
        auto pos = pts.end();
        size_t n_dom = 0;
        bool first_dom = true;
        for (auto p = pts.begin() ; p != pts.end() ; ++p) {
            if (p->x < x && p->y <= y) return false; // dominated
            if (p->x < x) continue;
            // x <= p->x
            if (y <= p->y) { // *p is dom
                ++n_dom;
                if (first_dom) {
                    pos = p;
                    first_dom = false;
                }
            } else {
                if (first_dom) {
                    return false;
                } else if (pos != pts.end()) {
                    std::swap(*pos, *p);
                    ++pos;
                }
            }
        }
        if (first_dom) {
            return false;
        } else {
            pts.resize(pts.size() - n_dom);
            return true;
        }
    }

    void clear() { pts.clear(); }

    void print(std::ostream &cout = std::cout) {
        for (auto p : pts) {
            cout << p.x <<","<< p.y <<" ";
        }
        cout <<"\n";
    }

    void check() {
        W x, y;
        bool first = true;
        for (auto p : pts) {
            if (first) { first = false; }
            else { assert(x <= p.x && y >= p.y && (x < p.x || y > p.y)); }
            x = p.x; y = p.y;
        }
    }

    bool dominates(W x, W y) {
        auto pts_end = pts.end();
        for (auto p = pts.begin(); p != pts_end; ++p) {
            if (p->x <= x && p->y <= y) return true; // dominated
            if (p->x >= x) return false; // (y < p->y if x == p->x)
        }
        return false;
    }

    bool contains(W x, W y) {
        return std::any_of(pts.begin(), pts.end(),
                           [x,y](const point p) {
                               return p.x == x && p.y == y;
                           });
    }

};




namespace unit {

    void pareto_test(size_t n, int rnd = 100) {
        std::cerr <<"pareto_test: sizeof(int)="<< sizeof(int) <<"\n";
        pareto<int> ps(n);
        std::vector<pareto<int>::point> dom;
        for (int i = 0; i < n; ++i) {
            int x = rand() % rnd, y = rand() % rnd;
            std::cout <<"add "<< x <<","<< y <<"\n";
            bool dominated = ps.dominates(x, y);
            if ( ! ps.add(x, y)) {
                assert(dominated);
                dom.push_back(pareto<int>::point(x,y));
            } else {
                assert( ! dominated);
            }
            ps.print();
            ps.check();
            for (auto p : dom) assert(ps.dominates(p.x, p.y));
        }
        ps.add(9,4);
        ps.print();
        ps.check();
        for (auto p : dom) assert(ps.dominates(p.x, p.y));
    }
    
}
    


#endif // PARETO_HH
