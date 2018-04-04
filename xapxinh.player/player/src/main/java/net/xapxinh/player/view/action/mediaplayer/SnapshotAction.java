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

package net.xapxinh.player.view.action.mediaplayer;

import static net.xapxinh.player.Application.application;

import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;

import uk.co.caprica.vlcj.player.MediaPlayer;
import net.xapxinh.player.event.SnapshotImageEvent;
import net.xapxinh.player.view.action.Resource;

final class SnapshotAction extends MediaPlayerAction {

    SnapshotAction(Resource resource, MediaPlayer mediaPlayer) {
        super(resource, mediaPlayer);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        BufferedImage image = mediaPlayer.getSnapshot();
        if (image != null) {
            application().post(SnapshotImageEvent.instance(image));
        }
    }
}
