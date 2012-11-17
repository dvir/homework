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
 
using namespace std;

vector<string> str_split(string str, char separator) {
	vector<string> words;
	string word = ""; 
    string::iterator it;
	for (it = str.begin(); it < str.end(); ++it) {
		if (*it == separator) {
			words.push_back(word);
			word = "";
		} else {
			word = word + *it;
		}
	}

	if (word != "") words.push_back(word);
	
	return words;
}

int main(int argc, char* argv[]) {
    srand(time(NULL));

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

    int semesters = 0;
    int CS_elective_courses = 0;
    int PG_elective_courses = 0;

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
        data = str_split(line, '=');    
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
        data = str_split(line, ',');    
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
    
    while (getline(conf, line)) {
        data = str_split(line, ',');    
        Student* student;
        if (data[1] == "CS") {
            student = new CSStudent(data, CS_elective_courses);
        } else if (data[1] == "PG") {
            student = new PGStudent(data, PG_elective_courses);
            if (!malag) {
                std::cout << student->getId() << " is being denied his education." << std::endl;
            }
        }
        
        students.push_back(student);
    }
    
    conf.close();

    // open output log file
    ofstream log;
    log.open("random.log");
    if (!log.is_open()) {
        cout << "Unable to open log file." << endl;
        return 1;
    }
    
    // start simulation
    for (size_t semester = 1; semester <= semesters; ++semester) {
        cout << "Semester " << semester << " of Random University." << endl;
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

            student->startSemester(semester);

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
                        course->reg(*student);
                    }
                }
            }
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

    // announce graduation status
    size_t dept_courses_count = 0;
    for (Students::iterator it = students.begin();
         it < students.end();
         ++it)
    {
        Student* student = *it;
        if (student->getDept() == "CS") {
            dept_courses_count = CS_courses.size();
        } else if (student->getDept() == "PG") {
            dept_courses_count = PG_courses.size();

            // if the MALAG didn't allow the student to study,
            // we shouldn't produce any output for such students.
            if (!malag) {
                continue;
            }
        }

        if (student->hasGraduated(dept_courses_count)) {
            std::cout << student->getId() << " has graduated" << std::endl;
        } else {
            std::cout << student->getId() << " has not graduated" << std::endl;
        }
    }

    ImageLoader img1("Lenna.png");
    img1.displayImage();
                         
    ImageOperations opr;
                              
    ImageLoader img2(100,100);
    opr.resize(img1.getImage(),img2.getImage());
    img2.displayImage();

    ImageLoader img3(img1.getImage().size().height, img1.getImage().size().width * 2);
    opr.copy_paste_image(img1.getImage(),img3.getImage(),0);
    opr.copy_paste_image(img1.getImage(),img3.getImage(),img1.getImage().size().width);                                                                                   img3.displayImage();

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
