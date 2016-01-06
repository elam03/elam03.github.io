(function() {
    var app = angular.module('elam03.github.io.app', ['ui.bootstrap', 'ngAnimate', 'ngTouch']);

    var project_data;

    app.factory('ProjectFactory', function($q, $http) {

        var projectList = [];

        var service = {
            getProjects: getProjects
        };

        return service;

        function getProjects(refresh) {
            var preview_path = '1gam_projects';
            if (refresh || !this.projectList) {
                return $http.get(preview_path + '/project_list.json').then(function(data) {
                    this.projectList = data.data;

                    // Adjust the image links so that they are correct.
                    for (var i = 0; i < this.projectList.length; ++i) {
                        for (var j = 0; j < this.projectList[i].previews.length; ++j) {
                            this.projectList[i].previews[j] = preview_path + '/' + this.projectList[i].previews[j];
                        }
                    }

                    return this.projectList;
                })
            } else {
                var deferrer = $q.defer();
                deferrer.resolve(rhis.projectList);
                return deferrer.promise;
            }
        }
    });

    app.controller('ProjectListController', ["$scope", "$http", "ProjectFactory", function($scope, $http, ProjectFactory) {
        $scope.oneAtATime = true;

        ProjectFactory.getProjects().then(function(projects){
            $scope.projects = projects;
        }, function(err){
            console.log('uh oh!');
        });
    }]);

    app.controller('DropdownCtrl', function($scope) {
        $scope.items = [
            "The first choice!",
            "And another choice for you.",
            "but wait! A third!"
        ];
    });

    app.controller('TabsController', function($scope, $window) {
        $scope.tabs = [
            { title:'Interests', content:'My interests...' },
            // { title:'Dynamic Title 2', content:'Dynamic content 2', disabled: true }
        ];

        $scope.alertMe = function() {
            setTimeout(function() {
                $window.alert('You\'ve selected the alert tab!');
            });
        };

        // $scope.tabs[0].active = true;
    });

    app.controller('ProjectsCarouselCtrl', function ($http, $scope, ProjectFactory) {
        ///////////////////////////////////////////////////////////////////////

        $scope.myInterval = 5000;
        $scope.noWrapSlides = false;
        var slides = $scope.slides = [];

        $scope.addSlide = function(title, img, link) {
            var newWidth = 600 + slides.length + 1;
            slides.push({
                image: (!img) ? ('//placekitten.com/' + newWidth + '/300') : (img),
                text: title,
                link: link
            });
        };

        ProjectFactory.getProjects().then(function(projects){
            $scope.projects = projects;

            for (var i = 0; i < projects.length; ++i) {
                $scope.addSlide(projects[i].title, projects[i].previews[0], projects[i].content);
                for (var j = 1; j < projects[i].previews.length; ++j) {
                    $scope.addSlide(projects[i].title, projects[i].previews[j]);
                }
            }
        }, function(err){
            console.log('uh oh!');
        });
    });
})();
