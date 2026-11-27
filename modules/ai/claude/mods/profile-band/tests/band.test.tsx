import { expect, test } from 'claude-code/testing'

const start = async ($: any, on: any, configDir: string) => {
  on('process.run', () => ({
    value: { exitCode: 0, stdout: `/Users/me\n${configDir}`, stderr: '' },
  }))
  on('session.start', (_: unknown, e: { cwd: string }) => ({ cwd: e.cwd }))
  await $.session.start({ cwd: '/', surface: 'terminal', isInteractive: true })
}

const cases = [
  ['', 'PERSONAL'],
  ['/Users/me/.claude', 'PERSONAL'],
  ['/Users/me/.claude-work', 'WORK'],
] as const

for (const [configDir, label] of cases) {
  for (const surface of ['terminal', 'desktop'] as const) {
    test(`CLAUDE_CONFIG_DIR=${configDir || '(unset)'} shows ${label} on ${surface}`, async ($, on) => {
      await start($, on, configDir)
      const ui = await $.ui.mount({ plugin: 'profile-band', surface, component: 'AbovePrompt', props: {} as any })
      expect((await ui.find({ type: 'Text', text: new RegExp(label) }))?.text).toBe(label)
      await ui.unmount()
    })
  }
}
