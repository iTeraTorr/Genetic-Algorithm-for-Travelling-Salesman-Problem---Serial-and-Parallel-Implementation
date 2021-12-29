#include<bits/stdc++.h>
#include<omp.h>//I am using openmp in this parallel program
using namespace std;
#define db double
#define rep(i,n) for(int i=0;i<n;i++)
#define pb push_back
#define all(a) a.begin(), a.end()

int num_of_threads; //represents the number of threads to be used in this parallel program

//template that splices a vector and returns that spliced vector
template<typename T>
vector<T> s(vector<T> const &v, int m, int n) {
   auto first = v.begin() + m;
   auto last = v.begin() + n + 1;
   vector<T> vector(first, last);
   return vector;
}

//Class that represents a City with (x,y) corrdinate in a cartesian plane carring an id that is unique
class City
{
    public:

    //Data being carried by class City
    db x;
    db y;

    //Default constructor of City class
    City(){
        x=0;
        y=0;
    }

    //Initializing a City class
    City(db a, db b){
        this->x=a;
        this->y=b;
    }

    //Member function to calculate distance between two cities
    db distance(City a){
        db xDis=abs(a.x-this->x);
        db yDis=abs(a.y-this->y);
        return sqrt(xDis*xDis + yDis*yDis);
    }

    //Member function to calculate distance of city from origin
    db distanceFromOrigin(){
        return sqrt(x*x + y*y);
    }

    //Member function to print the information of a city
    void value(){
        cout<<"("<<x<<", "<<y<<")\n";
    }

    //Operator overloading for comparison between two cities
    friend bool operator== (const City &c1, const City &c2);
    friend bool operator< (const City &c1, const City &c2);
};

//Two cities are equal if their distance from origin is equal
bool operator== (const City &c1, const City &c2){
    return sqrt(c1.x*c1.x + c1.y*c1.y) == sqrt(c2.x*c2.x + c2.y*c2.y);
}

bool operator< (const City &c1, const City &c2){
    return sqrt(c1.x*c1.x + c1.y*c1.y) < sqrt(c2.x*c2.x + c2.y*c2.y);
}

//This class analyzes how fit a route is, calculates distance and fitness value of a route
class Fitness{
    public:
    vector<City> route;
    db distance;
    db fitness;

    Fitness(vector<City> r){
        route=r;
        distance=0;
        fitness=0;
    }


    //Member function to calculate distance of a route
    db routeDistance(){
        if(distance==0){
            db pathDistance=0;
            int l=route.size();
            for(int i=0;i<l;i++){
                City fromCity=route[i];
                City toCity;
                if(i<l-1)
                    toCity=route[i+1];
                else
                    toCity=route[0];
                pathDistance+=fromCity.distance(toCity);
            }
            this->distance=pathDistance;
        }
        return distance;
    }


    //Member function to calculate fitness value of a route, i.e. 1/routeDistance
    db routeFitness(){
        if(fitness==0){
            fitness=1.0/routeDistance();
        }
        return fitness;
    }
};

//It creates a random route from a given citylist
vector<City> createRoute(vector<City> cityList){
    unsigned seed = std::chrono::system_clock::now()
                        .time_since_epoch()
                        .count();
    shuffle (cityList.begin(), cityList.end(), std::default_random_engine(seed));
    return cityList;
}

//It creates a random population of routes
vector<vector<City>> initialPopulation(int popSize, vector<City> cityList){
    vector<vector<City>> population(popSize);

    //Parallelized the code here
    #pragma omp parallel for
    rep(i,popSize){
        vector<City> temp=createRoute(cityList);
        population[i]=temp;
    }
    return population;
}

//Comparison function
bool comp(pair<int,db> &a, pair<int,db> &b){
    return a.second>b.second;
}

//This function rank the population based on fitness value
vector<pair<int,db>> rankRoutes(vector<vector<City>> population){
    int l=population.size();
    vector<pair<int,db>> fitnessResults(l);
    rep(i,l){
        fitnessResults[i].second=Fitness(population[i]).routeFitness();
        fitnessResults[i].first=i;
    }
    sort(all(fitnessResults),comp);
    
    return fitnessResults;
}


//This function does selection on the population and selects the chromosomes that are fittest in the popution
vector<int> selection(vector<pair<int,db>> popRanked, int eliteSize){
    vector<int> selectionResults;
    int n=popRanked.size();
    vector<vector<db>> cum(n,vector<db>(2));
    db sum=0;
    rep(i,n){
        sum+=popRanked[i].second;
        cum[i][0]=sum;
    }
    rep(i,n){
        cum[i][1]=(100*cum[i][0])/sum;
    }

    //Eliticism is maintained accross generations
    rep(i,eliteSize){
        selectionResults.pb(popRanked[i].first);
    }

    //Remaining popution is selected randomly using roulette wheel algorithm
    rep(i,n-eliteSize){
        db pick = 100*((double) rand() / (RAND_MAX));
        rep(j,n){
            if(pick <= cum[j][1]){
                selectionResults.pb(popRanked[j].first);
                break;
            }
        }
    }
    return selectionResults;
}

//This function creates matingPool that can breed together to generate next generation
vector<vector<City>> matingPool(vector<vector<City>> population, vector<int> selectionResults){
    int n=selectionResults.size();
    vector<vector<City>> matingpool;
    rep(i,n){
        int index=selectionResults[i];
        matingpool.pb(population[index]);
    }
    return matingpool;
}

//Fucntion that does cross breeding of two parents from the parent pool
vector<City> breed(vector<City> parent1, vector<City> parent2){
    vector<City> child;
    int n=parent1.size();
    
    //Randomization in breeding
    int geneA=(int)(((double) rand() / (RAND_MAX))*((double)n));
    int geneB=(int)(((double) rand() / (RAND_MAX))*((double)n));
    
    int startGene=min(geneA,geneB);
    int endGene=max(geneA,geneB);
    set<City> se;
    for(int i=startGene;i<endGene;i++){
        child.pb(parent1[i]);
        se.insert(parent1[i]);
    }

    rep(i,n){
        if(se.find(parent2[i])==se.end()){
            child.pb(parent2[i]);
        }
    }
    return child;
}

//Function that does cross breeding among the complete parent population in order to create new population
vector<vector<City>> breedPopulation(vector<vector<City>> matingpool, int eliteSize){
    vector<vector<City>> children;
    int n=matingpool.size();
    vector<vector<City>> pool(matingpool);
    int length=n-eliteSize;
    unsigned seed=std::chrono::system_clock::now()
                        .time_since_epoch()
                        .count();
    shuffle(pool.begin(), pool.end(), std::default_random_engine(seed));
    rep(i,eliteSize){
        children.pb(matingpool[i]);
    }

    rep(i,length){
        children.pb(breed(pool[i],pool[n-1-i]));
    }
    return children;
}

//Fucntion that does mutations in an individual route with the given mutation rate
vector<City> mutate(vector<City> individual, db mutationRate){
    int n=individual.size();
    rep(swapped,n){
        if(((double) rand() / (RAND_MAX)) < mutationRate){
            int swapWith=int(((double) rand() / (RAND_MAX))*((double)n));
            swap(individual[swapped], individual[swapWith]);
        }
    }
    return individual;
}

//Function that mutates the complete complete population
vector<vector<City>> mutatePopulation(vector<vector<City>> population, db mutationRate){
    int n=population.size();
    vector<vector<City>> mutatedPop(n);

    //Parallelization of code here
    #pragma omp parallel for
    rep(i,n){
        vector<City> mutatedInd=mutate(population[i],mutationRate);
        mutatedPop[i]=mutatedInd;
    }
    return mutatedPop;
}

//This function creates the nextGeneration from the existing popution
vector<vector<City>> nextGeneration(vector<vector<City>> currentGen, int eliteSize, db mutationRate){
    
    //Firstly the popution is ranked on the basis of the fitness of each route
    vector<pair<int,db>> popRanked=rankRoutes(currentGen);

    //Selection is then done to ensure survival of the fittest
    vector<int> selectionResults=selection(popRanked,eliteSize);

    //Mating pool is created where selected parent are selected to breed together to create new individuals
    vector<vector<City>> matingpool=matingPool(currentGen,selectionResults);

    //Complete population is cross breeded to obtain the next generation
    vector<vector<City>> children=breedPopulation(matingpool,eliteSize);

    //Mutations are done to the complete population in order to ensure diversity
    return mutatePopulation(children,mutationRate);
}

//Main genetic algorithm starts here
/*
Parameters that it considers:
cityList=List of cities that will be randomized in order to create random routes
popSize=number of routes in the initial population
eliteSize=top eliteSize routes will be passed to the next generation during selection to maintain elitism
mutationRate=%mutation in the new generation to ensure diversity in the population
generations=number of generations that is to be tracked for the population of routes
*/
vector<City> geneticAlgorithm(vector<City> cityList, int popSize, int eliteSize, db mutationRate, int generations){
    
    //Creating initial random popution
    vector<vector<City>> pop= initialPopulation(popSize, cityList);
    
    vector<db> progress;
    cout<<"Initial Best Distance: "<<1.0/((rankRoutes(pop))[0].second)<<"\n\n";
    
    //Holds the best route's path length of each generation
    progress.pb(1.0/((rankRoutes(pop))[0].second));
    
    //Work that is to be done by each thread
    int work=ceil((double)popSize/num_of_threads);
    
    //Generations starts..
    rep(j,generations){

        //Vector that will hold the next generation
        vector<vector<City>> nextPop;

        //Initialize the number of threads in this parallel program
        omp_set_num_threads(num_of_threads);
        //Creating a team of threads
        #pragma omp parallel
        {   
            #pragma omp parallel for
            for(int i=0;i<num_of_threads;i++){
                vector<vector<City>> localPop=s(pop,i*work,min(i*work+work-1,popSize-1));
                
                //Creating next generation
                localPop=nextGeneration(localPop,eliteSize,mutationRate);
                
                //Critical Section
                #pragma omp critical
                nextPop.insert(nextPop.end(), localPop.begin(), localPop.end());
                //Critical Section Ends
            }

            #pragma omp master
            cerr<<j<<"\n";
        }
        pop=nextPop;
        progress.pb(1.0/((rankRoutes(pop))[0].second));
        // cout<<i<<" : "<<1.0/((rankRoutes(pop))[0].second)<<"\n";
    }
    
    int siz=progress.size();

    rep(i,siz){
        cout<<i<<" : "<<progress[i]<<"\n";
    }

    cout<<"\nFinal Best Distance: "<<1.0/((rankRoutes(pop))[0].second)<<"\n";

    //This is the index for the best route in the population
    int bestRouteIndex=rankRoutes(pop)[0].first;

    //Best route is returned
    return pop[bestRouteIndex];
}

//Main function starts...
int main(){

    //Current time is used as seed
    srand(time(0));
    
    cout<<"Enter number of Cities: ";
    int k;
    cin>>k;
    cout<<"Enter number of threads: ";
    cin>>num_of_threads;
    
    //Vector to hold the list of cities to be taken into consideration
    vector<City> cityList(k);
    
    cout<<"Cities:\n\n";
    rep(i,k){

        //Cities are being given as input from stdin
        // int a,b;
        // cin>>a>>b;
        // cityList.pb(City(a,b));

        //Cities are being generated randomly
        cityList[i]=City((int)(200*((double) rand() / (RAND_MAX))),(int)(200*((double) rand() / (RAND_MAX))));
        cityList[i].value();
    }

    //Holds the best route for TSP_GA
    vector<City> bestCityList=geneticAlgorithm(cityList,1500,80,0.01,500);
    return 0;
}