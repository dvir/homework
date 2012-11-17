#ifndef PGCOURSE_H
#define PGCOURSE_H
#include "course.h"
class PGCourse : public Course {
    public:
        PGCourse(std::vector<std::string> data);
        virtual void reg(Student& s);
};
#endif
