export type Profile = { label: string; color: string }

declare module 'claude-code' {
  interface PluginState {
    'profile-band': { profile: Profile | null }
  }
}
