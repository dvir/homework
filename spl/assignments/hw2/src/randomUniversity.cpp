#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cstdlib>
#include <ctime>

#include "../include/typedef.h"

#include "../include/course.h"
#include "../include/cscourse.h"
#include "../include/pgcourse.h"
#include "../include/electivecourse.h"

#include "../include/student.h"
#include "../include/csstudent.h"
#include "../include/pgstudent.h"

#include "../include/imageloader.h"
#include "../include/imageoperations.h"

#include "../include/utils.h"
 
using namespace std;

int main(int argc, char* argv[]) {
    srand(time(NULL));

    // clean log file
    ofstream log;
    log.open("random.log");
    if (!log.is_open()) {
        cout << "Unable to open log file." << endl;
        return 1;
    }
    log.close();

    ifstream conf;
    string line;
    vector<string> data;
    bool malag = true;

    if (argc > 1) {
        if (string(argv[1]) == "MALAG=no") {
            malag = false;
        }
    }

    Courses CS_courses;
    Courses PG_courses;
    Courses elective_courses;

    Students students;

    size_t semesters = 0;
    size_t CS_elective_courses = 0;
    size_t PG_elective_courses = 0;

    conf.open("curriculum.conf");
    if (!conf.is_open()) {
        cout << "Unable to read curriculum configuration." << endl;
        return 1;
    }

    if (getline(conf, line)) {
        // first line consists of NUMBER_OF_SEMESTERS=x
        semesters = atoi(line.substr(line.find("=")+1).c_str());
    }
    
    // read dept,required_elective_courses data
    while (getline(conf, line)) {
        data = Utils::str_split(line, ',');    
        if (data[0] == "CS") {
            CS_elective_courses = atoi(data[1].c_str());
        } else if (data[0] == "PG") {
            PG_elective_courses = atoi(data[1].c_str());
        }
    }
    
    conf.close();
    
    conf.open("courses.conf");
    if (!conf.is_open()) {
        cout << "Unable to read courses configuration." << endl;
        return 1;
    }

    while (getline(conf, line)) {
        data = Utils::str_split(line, ',');    
        if (data[0] == "CS") {
            CS_courses.push_back(new CSCourse(data));
        } else if (data[0] == "PG") {
            PG_courses.push_back(new PGCourse(data));
        } else if (data[0] == "ELECTIVE") {
            elective_courses.push_back(new ElectiveCourse(data));
        }
    }

    conf.close();
    
    conf.open("students.conf");
    if (!conf.is_open()) {
        cout << "Unable to read students configuration." << endl;
        return 1;
    }
    
    log.open("random.log", ios::app);
    if (!log.is_open()) {
        cout << "Unable to open log file." << endl;
        return 1;
    }
    while (getline(conf, line)) {
        data = Utils::str_split(line, ',');    
        Student* student;
        if (data[1] == "CS") {
            student = new CSStudent(data, CS_elective_courses);
        } else if (data[1] == "PG") {
            student = new PGStudent(data, PG_elective_courses);
            if (!malag) {
                Utils::log(student->getId(), " is being denied his education.");
            }
        }
        
        Students::iterator it = students.begin();
        for ( ; it < students.end() && (*it)->getId() < student->getId();
             ++it)
        {
            //iterator stops in the place
            //where we want to insert the new student
            //(we are sorting the students' vector)
        }
        students.insert(it, student);
    }
    log.close(); 
    conf.close();

    // start simulation
    for (size_t semester = 1; semester <= semesters; ++semester) {
        Utils::log("Semester ", semester, " of Random University.");
        
        // for each student, find out what is the latest semester
        // he has finished all the courses for.
        // if the next semester he should participate on is odd or even
        // together with the global semester counter, register him
        // to all of the department courses this semester.
        //
        // ALSO: if he still needs to take elective courses, register him
        // to Student.getElectiveCoursesCount() amount of elective courses.
        
        for (Students::iterator it = students.begin(); 
             it < students.end(); 
             ++it) 
        {
            Student* student = *it;
            
            // if the MALAG didn't allow the student to study,
            // we shouldn't produce any output for such students.
            if (student->getDept() == "PG" && !malag) {
                continue;
            }

            if (!student->hasSemesterCoursesLeft()) {
                Courses* deptCourses;
               
                if (student->getDept() == "CS") {
                    deptCourses = &CS_courses;
                } else if (student->getDept() == "PG") {
                    deptCourses = &PG_courses;
                }
                
                for (Courses::iterator it2 = (*deptCourses).begin();
                     it2 < (*deptCourses).end();
                     ++it2)
                {
                    Course* course = *it2;
                    if (course->getSemester() == student->getCurrentSemester())
                    {
                        student->addSemesterCourse(*course);
                    }
                }
            }

            int elective_courses_count = student->getElectiveCoursesCount();
            if (elective_courses_count > 0) {
                for (Courses::iterator it2 = elective_courses.begin();
                     it2 < elective_courses.end() 
                     && elective_courses_count > 0;
                     ++it2)
                {
                    Course* course = *it2;
                    if (semester%2 == course->getSemester()%2
                        && !student->hasCompleted(*course)) 
                    {
                        student->addElectiveCourse(*course);
                        elective_courses_count--;
                    }
                }
            }
            
            student->startSemester(semester);
        }

        for (Courses::iterator it = CS_courses.begin();
             it < CS_courses.end();
             ++it)
        {
            (*it)->teach();
        }
        
        if (malag) {
            for (Courses::iterator it = PG_courses.begin();
                 it < PG_courses.end();
                 ++it)
            {
                (*it)->teach();
            }
        }

        for (Courses::iterator it = elective_courses.begin();
             it < elective_courses.end();
             ++it)
        {
            (*it)->teach();
        }
    }

    // open output log file    
    log.open("random.log", ios::app);
    if (!log.is_open()) {
        cout << "Unable to open log file." << endl;
        return 1;
    }

    // announce graduation status
    size_t dept_courses_count = 0;
    for (Students::iterator it = students.begin();
         it < students.end();
         ++it)
    {
        Student* student = *it;
        if (student->getDept() == "CS") {
            dept_courses_count = CS_courses.size() + CS_elective_courses;
        } else if (student->getDept() == "PG") {
            dept_courses_count = PG_courses.size() + PG_elective_courses;

            // if the MALAG didn't allow the student to study,
            // we shouldn't produce any output for such students.
            if (!malag) {
                continue;
            }
        }

        if (student->hasGraduated(dept_courses_count)) {
            Utils::log(student->getId(), " has graduated");
        } else {
            Utils::log(student->getId(), " has not graduated");
        }
    }
    log.close();

    size_t CS_students_count=0;
    size_t PG_students_count=0;

    for (Students::iterator it = students.begin();
         it < students.end();
         ++it)
    {
        if ((*it)->getDept() == "CS") {
            ++CS_students_count;
        }
        else{
            ++PG_students_count;
        }
    }

    if (argc < 3) {
        ImageOperations opr;
        ImageLoader CS_image(100, 100*CS_students_count);
        ImageLoader PG_image(100, 100*PG_students_count);
        ImageLoader frame(100, 100);
        size_t i = 0, j = 0;
        for (Students::iterator it = students.begin();
             it < students.end();
             ++it)
        {
            Student* student = *it;
            ImageLoader current_student(100, 100); // frame
            ImageLoader source_image(student->getImage());
            opr.resize(source_image.getImage(), current_student.getImage());
            
            if (student->getDept() == "CS"){
                //turn to greyscale if needed
                if (!student->hasGraduated(CS_courses.size() + CS_elective_courses)) {
                    opr.rgb_to_greyscale(current_student.getImage(), current_student.getImage());
                }
                opr.copy_paste_image(current_student.getImage(), CS_image.getImage(), 100*i);
                ++i;
            } else {
                //turn to greyscale if needed
                if (!student->hasGraduated(PG_courses.size() + PG_elective_courses)) {
                    opr.rgb_to_greyscale(current_student.getImage(), current_student.getImage());
                }
                opr.copy_paste_image(current_student.getImage(), PG_image.getImage(), 100*j);
                ++j;
            }
        }

        //save to files
        CS_image.saveImage("CS.jpg");
        PG_image.saveImage("PG.jpg");

        // display images
        CS_image.displayImage();
        PG_image.displayImage();
    }
    
    // clean everything!
    for (Courses::iterator it = CS_courses.begin();
         it < CS_courses.end();
         ++it)
    {
        delete *it;
        *it = 0;
    }
    
    for (Courses::iterator it = PG_courses.begin();
         it < PG_courses.end();
         ++it)
    {
        delete *it;
        *it = 0;
    }
    

    for (Courses::iterator it = elective_courses.begin();
         it < elective_courses.end();
         ++it) 
    {
        delete *it;
        *it = 0;
    }

    for (Students::iterator it = students.begin();
         it < students.end();
         ++it)
    {
        delete *it;
        *it = 0;
    }
}
