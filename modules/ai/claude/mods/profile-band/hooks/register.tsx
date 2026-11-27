import { atom, read, update } from 'claude-code'
import type { Register } from 'claude-code'

import type { Profile } from '../types'

const profile = atom({ plugin: 'profile-band', key: 'profile' } as const, null)

const PERSONAL = 'suggestion'
const WORK = 'warning'
const OTHER = 'merged'

const detect = (configDir: string, home: string): Profile => {
  const dir = configDir.replace(/\/+$/, '') || `${home}/.claude`
  if (dir === `${home}/.claude`) {
    return { label: 'PERSONAL', color: PERSONAL }
  }
  if (dir === `${home}/.claude-work`) {
    return { label: 'WORK', color: WORK }
  }
  const name = dir.split('/').pop() ?? dir
  return { label: name.replace(/^\./, '').toUpperCase(), color: OTHER }
}

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    const started = await next(e)
    const { stdout } = await $.process.run([
      'sh',
      '-c',
      'printf "%s\\n%s" "$HOME" "$CLAUDE_CONFIG_DIR"',
    ])
    const [home = '', configDir = ''] = stdout.split('\n')
    const found = detect(configDir, home)
    await update($, profile, () => found)
    return started
  })

  on('ui.render', { component: 'AbovePrompt' }, async ($, e, next) => {
    const current = await read($, profile)
    if (current === null || e.props.hasSurvey) {
      return next(e)
    }

    const { Box, Text } = $.ui.resolve(e)

    return (
      <Box flexDirection="row" alignItems="center" gap={1}>
        <Text color={current.color}>◆</Text>
        <Text dimColor>Profile</Text>
        <Text bold color={current.color}>
          {current.label}
        </Text>
      </Box>
    )
  })
}
