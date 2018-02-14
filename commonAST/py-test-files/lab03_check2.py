def soccer(wins, losses, draw, goals_for, goals_against, team_name):
    points = (wins * 3) + draw
    goal_adv = goals_for - goals_against
    print(team_name)
    print("Win:", wins, 'Lose:', losses, 'Draw:', draw)
    print('Total number of points:', points, "Goal advantage:", goal_adv)

soccer(2, 1, 0, 7, 2, 'Germany')

soccer(1, 1, 1, 4, 4, 'USA')

soccer(3, 0, 0, 6, 3, 'Argentina')

soccer(0, 1, 2, 2, 4, 'England')