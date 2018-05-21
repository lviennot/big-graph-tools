cmake_minimum_required(VERSION 3.10)
project(hublab)

set(CMAKE_CXX_STANDARD 11)


include_directories(src)
add_executable(hltrans
        HL_trans.cc
        edge.hh
        mgraph.hh
        traversal.hh
        pruned_landmark_labeling.hh
)
target_link_libraries(hltrans z)
