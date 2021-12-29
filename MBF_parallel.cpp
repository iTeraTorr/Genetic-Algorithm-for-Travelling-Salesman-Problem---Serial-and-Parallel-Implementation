#include<bits/stdc++.h>
#include<omp.h> //I am using openmp in this parallel program
using namespace std;
#define db double
#define rep(i,n) for(int i=0;i<n;i++)
#define pb push_back
#define all(a) a.begin(), a.end()
#define Vpll vector<pair<int,int>>

int num_of_threads; //represents the number of threads to be used in this parallel program

class Cichlid;
class Fitness;

//Class that represents a City with (x,y) corrdinate in a cartesian plane carring an id that is unique
class City
{
    public:
    //Data being carried by class City
    db x;
    db y;
    int id;

    //Default constructor of City class
    City(){
        x=0;
        y=0;
    }

    //Initializing a City class
    City(db a, db b, int id1){
        this->x=a;
        this->y=b;
        this->id=id1;
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
        cerr<<id<<": ("<<x<<", "<<y<<")\n";
    }

    //Operator overloading for comparison between two cities
    friend bool operator== (const City &c1, const City &c2);
    friend bool operator< (const City &c1, const City &c2);
};

//Two cities are equal if their ids are equal, since each city has a unique id
bool operator== (const City &c1, const City &c2){
    return (c1.id == c2.id);
}

bool operator< (const City &c1, const City &c2){
    return (c1.id < c2.id);
}

//Function that is useful in calculating distance between two
void visit(int i,vector<bool> &Isvisit, vector<int> &r1, vector<int> &seq, map<int,int> &mp){
    if(Isvisit[i])
    return;
    Isvisit[i]=true;
    seq.pb(mp[r1[i]]);
    visit(mp[r1[i]],Isvisit,r1,seq,mp);
}

//This class analyzes how fit a route is, calculates distance and fitness value of a route
class Fitness{
    public:
    vector<City> path;
    db distance;
    db fitness;

    Fitness(vector<City> r){
        path=r;
        distance=0;
        fitness=0;
    }


    //Member function to calculate distance of a route
    db routeDistance(){
        if(distance==0){
            db pathDistance=0;
            int l=path.size();
            for(int i=0;i<l;i++){
                City fromCity=path[i];
                City toCity;
                if(i<l-1)
                    toCity=path[i+1];
                else
                    toCity=path[0];
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


//This class is the major class that represent a solution/route
class Cichlid
{
    public:
    vector<City> route; //It carries the current route of Cichlid
    vector<City> bestRoute; //It carries the best route that cichlid has experienced in all its generations
    Vpll distanceAdd; //It stores the moves that the cichlid will move to reach its next generation of evolve
    
    Cichlid(){
    //Default Constructor
    }

    //Initializes the route and best route to the current route
    Cichlid(vector<City> r){
        route=r;
        bestRoute=r;
    }

    //Comparison operator overloading
    friend bool operator== (const Cichlid &c1, const Cichlid &c2);
    
    //Member function to move the Cichlid from current generation to the next generation
    void move(db sp,db natureForce){

        //Calculating the maximum number of moves that the cichlid can move based on the mother's source power and the natureForce value
        //Mother source power decreases in each generation but natureForce remains constant throughout
        int k=min(int(distanceAdd.size()), int(natureForce*sp));

        //Perform the movement
        //Movement means the sequence of swaps to be followed to reach to next generation
        for(int i=0;i<k;i++){
            swap(this->route[distanceAdd[i].first],this->route[distanceAdd[i].second]);
        }
    }


    //Member function to add a swap sequence to the existing swap sequence
    void add(Vpll swaps){
        distanceAdd.insert(distanceAdd.end(), swaps.begin(), swaps.end());
    }


    //Member function to calculate the swap sequence to reach to the destination Cichlid
    Vpll distanceTo(Cichlid destination){
        vector<int> r2,r1; //Creating vectors that store the id sequence of source and destination respectively
        
        for(auto x:destination.route){
            r1.pb(x.id);
        }
        for(auto x:this->route){
            r2.pb(x.id);
        }
        int n=r1.size();
        
        //It tracks which all cities are visited yet
        /*Algo- calculate the sequence of steps to be taken in going from one permutation(r1) to another(r2)*/
        
        vector<bool> isVisited(n,false); 
        int i=0;
        map<int,int> mp;
        rep(i,n){
            mp[r2[i]]=i;
        }
        Vpll swaps;
        while(i<n){
            if(!isVisited[i] && r1[i]!=r2[i]){
                vector<int> seq;
                seq.pb(i);
                visit(i,isVisited,r1,seq,mp);
                seq.pop_back();
                int l=seq.size();
                for(int j=l-2;j>=0;j--){
                    swaps.pb({seq[j],seq[j+1]});
                }
            }
            i++;
        }

        /*Algo ends*/
        return swaps;
    }

    //Member function to update the best route that the Cichlid has seen so far
    void updatePersonalBest(){
        if(Fitness(this->route).routeDistance() < Fitness(this->bestRoute).routeDistance())
            bestRoute=this->route;
    }

    //Member function to update the Cichlid along with the updation of personal and global best route
    //in order to reach to next genertion
    void update(Cichlid &globalBestRoute, db sp, db natureForce){
        
        //Probability that the personal best will make contribution in going to next generation
        db alpha=((double) rand() / (RAND_MAX));
        //Probability that the global best will make contribution in going to next generation
        db beta=((double) rand() / (RAND_MAX));

        //If alpha or beta is >=0.5 then only consider its contribution in deciding the path for next generation
        if(alpha>=0.5)
            alpha=1;
        else
            alpha=0;
        if(beta>=0.5)
            beta=1;
        else
            beta=0;

        //update personal best route        
        this->updatePersonalBest();

        //Calculate distance to personal best route and global best route
        Vpll per=this->distanceTo(bestRoute);
        Vpll glo=this->distanceTo(globalBestRoute);
        Vpll toAdd;

        //If none of personal or global wants to make contribution then make it move in the same direction that it was moving
        if(alpha==0 && beta==0){
            this->move(sp,natureForce);
        }else{

            if(alpha==1){
                toAdd.insert(toAdd.end(),per.begin(),per.end());
            }
            if(beta==1){
                toAdd.insert(toAdd.end(),glo.begin(),glo.end());
            }
            this->distanceAdd=toAdd;
            this->move(sp,natureForce);
        }
    }
};
bool operator== (const Cichlid &c1, const Cichlid &c2){
    return (c1.route == c2.route);
}
Cichlid globalBestRoute; //It carries the global best route

//It creates a random route/Cichlid from a given citylist
Cichlid createRoute(vector<City> cityList){
    unsigned seed = std::chrono::system_clock::now()
                        .time_since_epoch()
                        .count();
    shuffle (cityList.begin(), cityList.end(), std::default_random_engine(seed));
    return Cichlid(cityList);
}

//It creates a random population of Cichlids
vector<Cichlid> initialPopulation(int nFish, vector<City> cityList){
    vector<Cichlid> population(nFish);

    //Parallelized the code here
    #pragma omp parallel for
    rep(i,nFish){
        Cichlid temp=createRoute(cityList);
        population[i]=temp;
    }
    return population;
}

//Comparison function
bool comp(pair<int,db> &a, pair<int,db> &b){
    return a.second>b.second;
}

//This function rank the population based on fitness value
vector<pair<int,db>> rankRoutes(vector<Cichlid> population){
    int l=population.size();
    vector<pair<int,db>> fitnessResults(l);
    rep(i,l){
        fitnessResults[i].second=Fitness(population[i].route).routeFitness();
        fitnessResults[i].first=i;
    }
    sort(all(fitnessResults),comp);
    
    return fitnessResults;
}

//This function creates shark Attacks with frequency 90% (assumption) that selects the best among the population and leave the weak ones out
vector<int> sharkAttack(vector<pair<int,db>> popRanked){

    vector<int> sharkResults;
    int n=popRanked.size();
    int eliteSize=n/10; //Only 10% survives till next generation
    vector<vector<db>> cum(n,vector<db>(2));
    db sum=0;
    rep(i,n){
        sum+=popRanked[i].second;
        cum[i][0]=sum;
    }
    rep(i,n){
        cum[i][1]=(100*cum[i][0])/sum;
    }
    rep(i,eliteSize){
        sharkResults.pb(popRanked[i].first);
    }

    //Popuation from the remaining 90% is selected randomly to the next generation
    //Using roulette wheel algorithm
    rep(i,n-eliteSize){
        db pick = 100*((double) rand() / (RAND_MAX));
        rep(j,n){
            if(pick <= cum[j][1]){
                sharkResults.pb(popRanked[j].first);
                break;
            }
        }
    }
    return sharkResults;
}

//This function creates parentPool that can breed together to generate next generation
vector<Cichlid> parentPool(vector<Cichlid> population, vector<int> selectionResults){
    int n=selectionResults.size();
    vector<Cichlid> parentpool(n);
    //Parallelization in this section
    #pragma omp parallel for
    rep(i,n){
        int index=selectionResults[i];
        parentpool[i]=population[index];
    }
    return parentpool;
}

//Fucntion that does cross breeding of two parents from the parent pool
Cichlid cross(Cichlid parent1, Cichlid parent2){
    Cichlid child;
    int n=parent1.route.size();
    
    //Randomization in breeding
    int geneA=(int)(((double) rand() / (RAND_MAX))*((double)n));
    int geneB=(int)(((double) rand() / (RAND_MAX))*((double)n));
    
    int startGene=min(geneA,geneB);
    int endGene=max(geneA,geneB);
    set<City> se;
    for(int i=startGene;i<endGene;i++){
        (child.route).pb(parent1.route[i]);
        se.insert(parent1.route[i]);
    }

    rep(i,n){
        if(se.find(parent2.route[i])==se.end()){
            child.route.pb(parent2.route[i]);
        }
    }
    return child;
}

//Function that does cross breeding among the complete 
//parent population in order to create new population
vector<Cichlid> crossPopulation(vector<Cichlid> matingpool){
    vector<Cichlid> children;
    int n=matingpool.size();
    int eliteSize=n/10;
    vector<Cichlid> pool(matingpool);
    int length=n-eliteSize;
    unsigned seed=std::chrono::system_clock::now()
                        .time_since_epoch()
                        .count();
    shuffle(pool.begin(), pool.end(), std::default_random_engine(seed));
    rep(i,eliteSize){
        children.pb(matingpool[i]);
    }

    rep(i,length){
        children.pb(cross(pool[i],pool[n-1-i]));
    }
    return children;
}

//This function updates the global best route/Cichlid
void updateGlobalBest(vector<pair<int,db>> &popRanked, vector<Cichlid> &currentGen){
    globalBestRoute=currentGen[popRanked[0].first];
}

//This function creates the nextGeneration from the existing popution
vector<Cichlid> nextGeneration(vector<Cichlid> currentGen,db sp,db natureForce){
    
    //Firstly the popution is ranked on the basis of the fitness of each Cichlid
    vector<pair<int,db>> popRanked=rankRoutes(currentGen);
    
    //Global best route is updated
    updateGlobalBest(popRanked,currentGen);
    int n=currentGen.size();
    
    //Shark attacks are done in order to eliminate the weak population 
    vector<int> sharkResults=sharkAttack(popRanked);

    //Parent pool is created where pairs of parents breed to create new Cichlids
    vector<Cichlid> parentpool=parentPool(currentGen,sharkResults);

    //Actual breeding is done here
    vector<Cichlid> children=crossPopulation(parentpool);

    //Paralleliazation happens here
    #pragma omp parallel for
    for(int i=0;i<n;i++){
        //Each Cichlid is updated in order to become ready
        // to evolve into the next generation
        children[i].update(globalBestRoute,sp,natureForce);
    }

    //New popution is created and returned from this function
    return children;
}

//This function show the route captured by each Cichlid at the current moment
void show(Cichlid cich){
    for(auto a:cich.route){
        cerr<<a.id<<": ("<<a.x<<", "<<a.y<<")\n";
    }
}

//Main MBF Algorithm starts here
/*
Parameters that it considers
cityList=List of cities that will be randomized in order to create random Cichlids
nFish=number of Cichlids in the initial population
sp=Initial mother's source power
dis=Dispertion distance
pdis=Probability of dispersion to a distance 'dis'
spDamp=damping in the power that the mother experience after each generation
natureForce=movement in the Cichlids that the nature incorporates in the popution of Cichlids
nfe=number of generations that is to be tracked for the population of Cichlids
*/
Cichlid MBF(vector<City> cityList, int nFish, db sp, db dis, db pdis, db spDamp, int natureForce, int nfe){
    
    //Number of left out Cichlids that the mother is unable to brood
    int numberLeftOut=int(0.04*nFish*pow(sp,-0.431));
    
    //Creating initial random popution
    vector<Cichlid> pop=initialPopulation(nFish, cityList);

    cout<<"Initial Best Distance: "<<1.0/((rankRoutes(pop))[0].second)<<"\n\n";
    rep(i,nfe){
        vector<Cichlid> temp(pop);
        
        //Creating next generation
        pop=nextGeneration(pop,sp,natureForce);
        
        //Mother's power, i.e. source power decreses, i.e. damps after each generation
        sp=sp*spDamp;
        
        //Prints the best Cichlid's route's path length in the popution after each generation
        cerr<<i<<" : "<<1.0/((rankRoutes(pop))[0].second)<<"\n";
    }
    
    cout<<"\nFinal Best Distance: "<<1.0/((rankRoutes(pop))[0].second)<<"\n";

    //This is the index for the best Cichlid in the population
    int bestRouteIndex=rankRoutes(pop)[0].first;

    //Best Cichlid is returned
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
    
    //Initialize the number of threads in this parallel program
    omp_set_num_threads(num_of_threads);

    //Vector to hold the list of cities to be taken into consideration
    vector<City> cityList;
    
    cerr<<"Cities:\n\n";
    rep(i,k){

        //Cities are being given as input from stdin
        // int a,b;
        // cin>>a>>b;
        // cityList.pb(City(a,b,i));

        //Cities are being generated randomly
        cityList.pb(City((int)(200*((double) rand() / (RAND_MAX))),(int)(200*((double) rand() / (RAND_MAX))),i));
        cityList[i].value();
    }

    //Holds the best Cichlid for TSP_MBF
    Cichlid bestCichlid=MBF(cityList,100,1,1.5,0.7,0.95,25,1000);
    return 0;
}