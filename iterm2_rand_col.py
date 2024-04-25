#!/usr/bin/env python3.7

import iterm2
import random

async def main(connection):
    app = await iterm2.async_get_app(connection)
    window = app.current_terminal_window
    session = app.current_terminal_window.current_tab.current_session
    profile = await session.async_get_profile()
    if not window:
        return

    color_schemes = await iterm2.ColorPreset.async_get_list(connection)
    color_schemes = [scheme for scheme in color_schemes if 'light' not in scheme]
    random_color_scheme = random.choice(color_schemes)

    preset = await iterm2.ColorPreset.async_get(connection, random_color_scheme)

    await profile.async_set_color_preset(preset)

iterm2.run_until_complete(main)
