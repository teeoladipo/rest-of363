from psychopy import visual, core, event, clock
import random

win = visual.Window(size=(800, 600), color='grey')
fixcross = visual.TextStim(win, text='+', color='white')
square = visual.Rect(win, width=0.5, height=0.5, fillColor='white')
dot = visual.Circle(win, radius=0.1, fillColor='black')

num_trials = 2

for trial in range(num_trials):
    fixcross.draw()
    win.flip()
    core.wait(3)

    side = random.choice(['left', 'right'])
    if side == 'left':
        square.pos = (-0.3, 0)
    else:
        square.pos = (0.3, 0)

    for frame in range(60):
        square.draw()
        win.flip()

    dot.pos = square.pos 

    for frame in range(60):
        square.draw()
        dot.draw()
        win.flip()

    win.flip() 

    response_keys = event.waitKeys(keyList=["z", "slash"])

    start_time = core.getTime()

    keys = response_keys[0]
    response_time = core.getTime() - start_time

    if (side == 'left' and keys == 'z') or (side == 'right' and keys == 'slash'):
        response_correct = True
    else:
        response_correct = False

    print(f"Trial {trial + 1}: Key pressed: {keys}, Reaction Time: {response_time:.3f} sec, Correct: {response_correct}")

win.close()
core.quit()
