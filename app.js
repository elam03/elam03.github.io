(function() {
    var app = angular.module('elam03.github.io.app', ['ui.bootstrap', 'ngAnimate', 'ngTouch']);

    app.factory('DataService', function($q, $http) {
        var projectData = [];
        var detailsData = {};

        var service = {
            getProjects: getProjects,
            getDetailsData: getDetailsData
        };

        return service;

        function getProjects(refresh) {
            var preview_path = '1gam_projects';
            if (refresh || !this.projectData) {
                return $http.get(preview_path + '/project_list.json').then(function(data) {
                    this.projectData = data.data;

                    // Adjust the image links so that they are correct.
                    for (var i = 0; i < this.projectData.length; ++i) {
                        for (var j = 0; j < this.projectData[i].previews.length; ++j) {
                            this.projectData[i].previews[j] = preview_path + '/' + this.projectData[i].previews[j];
                        }
                    }

                    return this.projectData;
                })
            } else {
                var deferrer = $q.defer();
                deferrer.resolve(this.projectData);
                return deferrer.promise;
            }
        }

        function getDetailsData(refresh) {
            if (refresh || !this.detailsData) {
                return $http.get('details.json').then(function(data) {
                    this.detailsData = data.data;
                    return this.detailsData;
                })
            } else {
                var deferrer = $q.defer();
                deferrer.resolve(this.detailsData);
                return deferrer.promise;
            }
        }
    });

    app.controller('ProjectListController', ["$scope", "$http", "DataService", function($scope, $http, DataService) {
        $scope.oneAtATime = true;

        DataService.getProjects().then(function(projects){
            $scope.projects = projects;
        }, function(err){
            console.log('uh oh!');
        });
    }]);

    app.controller('TabsController', function($scope, $window, DataService) {
        // $scope.tabs = [
        //     { title:'Interests', content:'My interests...' },
        //     // { title:'Dynamic Title 2', content:'Dynamic content 2', disabled: true }
        // ];

        // style="border:2px solid black;margin"
        // style="height: 505px"
        // style="margin:auto;height:100%;max-height:100%
        DataService.getDetailsData().then(function(data) {
            $scope.tabs = data;
            $scope.details = data;
        }, function(err) {
            console.log('uh oh!');
        });
    });

    app.controller('ProjectsCarouselCtrl', function ($http, $scope, DataService) {
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

        DataService.getProjects().then(function(projects){
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
