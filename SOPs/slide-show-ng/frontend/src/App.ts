import { call, on } from './lib/bridge';

export type Slide = {
  label?: string;
  kind: string;
};

type SlideChangedEvent = {
  index: number;
};

export async function bootstrapDeckBridge(
  setCurrentSlide: (index: number) => void,
): Promise<Slide[]> {
  const slides = await call<Slide[]>('get_slides');

  on<SlideChangedEvent>('slide_changed', (data) => {
    setCurrentSlide(data.index);
  });

  return slides;
}
