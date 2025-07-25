# Definitely Not Fried Chicken Planner

This project is not affiliated with or endorsed by the creators of "Definitely Not Fried Chicken".

The code in this repository should provide a planning and optimisation tool for the game [Definitely Not Fried Chicken](https://store.steampowered.com/app/1036240/Definitely_Not_Fried_Chicken/). The goal is to generate optimal room layouts for different purposes and metrics.

For now it is a work in progress and pretty simple, you have to adjust the room size in the `Program.fs`. You can configure the amount of randomly generated rooms via command argument.

## Getting Started

To build the project, use:
```sh
dotnet build src/
```

To run the project, use:
```sh
cd src
dotnet run -- <number_of_rooms>
```

## Algorithm

The algorithm works for now as follows:
1. Generate a random room layout (emitters are turned up to the max)
2. Check, if the room layout is valid
3. Score the layout based on the number of emitters and their placement
4. If the score has enough potential to beat the currently best score, it will be optimized by tuning down the emitters as far as possible
5. Check, if the optimized layout is better than the current best layout
