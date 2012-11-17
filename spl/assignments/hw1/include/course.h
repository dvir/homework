#ifndef COURSE_H
#define COURSE_H
#include <string>
#include <vector>
#include <fstream>
#include "typedef.h"

class Course {
    public:
        Course(int weekday, std::string course_id, int room);
        bool addStudent(Student* student);
        void print(std::string filename);
        int getWeekday() const;
        std::string getCourseId() const;

    private:
        int _weekday;
        std::string _course_id;
        int _room;
        Students _students;
};
#endif
