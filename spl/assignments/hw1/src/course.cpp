#include <string>
#include <iostream>
#include <vector>
#include "../include/course.h"
#include "../include/student.h"
using namespace std;

Course::Course(int weekday, string course_id, int room):
    _weekday(weekday),
    _course_id(course_id),
    _room(room),
    _students()
{
    // constructor
}

bool Course::addStudent(Student* student) {
    if (_room <= 0) {
        // no more room in this course
        return false;
    }

    _students.push_back(student);
    student->addCourse(this);
    _room--;
    return true;
}

void Course::print(string filename) {
    ofstream stream;
    
    stream.open(filename.c_str(), ios::app);
    stream << _weekday << " " << _course_id << endl;
    for (size_t i = 0; i < _students.size(); ++i) {
        stream << _students[i]->getStudentId() << endl;
    }

    stream.close();
}

int Course::getWeekday() const {
    return _weekday;
}

string Course::getCourseId() const {
    return _course_id;
}
