#ifndef CSCOURSE_H
#define CSCOURSE_H
#include "course.h"
#include <string>
class CSCourse : public Course {
    public:
        CSCourse(std::vector<std::string> data);
        virtual void reg(Student& s);
};
#endif
