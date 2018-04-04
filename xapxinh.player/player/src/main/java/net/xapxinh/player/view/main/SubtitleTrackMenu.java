/*
 * This file is part of VLCJ.
 *
 * VLCJ is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * VLCJ is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with VLCJ.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2015 Caprica Software Limited.
 */

package net.xapxinh.player.view.main;

import static net.xapxinh.player.Application.application;
import static net.xapxinh.player.view.action.Resource.resource;

import java.util.List;

import javax.swing.Action;

import net.xapxinh.player.view.action.mediaplayer.SubtitleTrackAction;
import uk.co.caprica.vlcj.player.TrackDescription;

final class SubtitleTrackMenu extends TrackMenu {

    SubtitleTrackMenu() {
        super(resource("menu.subtitle.item.track"));
    }

    @Override
    protected Action createAction(TrackDescription trackDescription) {
        return new SubtitleTrackAction(trackDescription.description(), application().mediaPlayerPanel().getMediaPlayer(), trackDescription.id());
    }

    @Override
    protected List<TrackDescription> onGetTrackDescriptions() {
        return application().mediaPlayerPanel().getMediaPlayer().getSpuDescriptions();
    }

    @Override
    protected int onGetSelectedTrack() {
        return application().mediaPlayerPanel().getMediaPlayer().getSpu();
    }
}
